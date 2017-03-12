{-

sscan --- text UI for scanning with SANE

Copyright (C) 2017  Sean Whitton

This file is part of sscan.

sscan is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

sscan is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with sscan.  If not, see <http://www.gnu.org/licenses/>.

-}

{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.Async (concurrently_)
import           Control.Monad            (unless, void, when)
import           Data.Time.Clock.POSIX    (getPOSIXTime, posixSecondsToUTCTime)
import           Data.Time.Format         (defaultTimeLocale, formatTime,
                                           iso8601DateFormat)
import           Lens.Micro               ((^.))
import           System.Directory         (getHomeDirectory, removeFile,
                                           renamePath, withCurrentDirectory)
import           System.Exit              (ExitCode (..))
import           System.FilePath          ((<.>), (</>))
import           System.IO                (IOMode (WriteMode), hClose,
                                           hGetContents, hPutStr, openFile,
                                           withBinaryFile, withFile)
import           System.IO.Temp           (withSystemTempDirectory)
import           System.Process

import           Types.State
import           UI

processScanSessDir :: St -> FilePath -> IO ()
processScanSessDir st dir = withCurrentDirectory dir $ do
    posix <- getPOSIXTime
    let stamp = show (round posix :: Int)
    logH <- openFile (logFile stamp) WriteMode -- TODO maybe AppendMode?
    void $ case st^.stOutFormat of
      PDF -> do
          -- 1. convert tiff->PDF
          void $ createProcessWait_ "convert"
              (proc "convert" (allPages ++ ["temp.pdf"]))
          -- 2. set metadata with pdftk
          renamePath (dir </> "temp.pdf") (dir </> "temp2.pdf")
          writeFile "metadata" (metadata posix)
          void $ createProcessWait_ "pdftk"
              (proc "pdftk"
                ["temp2.pdf", "update_info", "metadata", "output", "temp.pdf"])
              { std_in = NoStream
              , std_out = NoStream
              , std_err = UseHandle logH
              }
          -- 3. maybe ocrmypdf
          when (st^.stOCR) $ do
              renamePath (dir </> "temp.pdf") (dir </> "temp2.pdf")
              -- OCRmyPDF dies if stdout is not connected, so tell it
              -- to output to stdout
              withBinaryFile "temp.pdf" WriteMode $ \tempFile -> do
                (_, _, Just herr, p) <- createProcess_ "OCRmyPDF"
                -- we'd like to use --remove-background here, but then
                -- qpdf says that ocrmypdf's output is damaged.
                -- reported upstream
                  (proc "ocrmypdf" ["-c", "-i", "-r", "temp2.pdf", "-"])
                  { std_in = NoStream
                  , std_out = UseHandle tempFile
                  , std_err = CreatePipe
                  }
                -- OCRmyPDF does not yet have a --quiet option, so we
                -- emulate the chronic(1) utility
                exitCode <- waitForProcess p
                unless (exitCode == ExitSuccess) $
                  hGetContents herr >>= hPutStr logH
                hClose herr
          -- 4. qpdf (ocrmypdf invokes qpdf but it doesn't use
          -- --linearize, which shrinks the PDF, often substantially)
          void $ createProcessWait_ "qpdf"
              (proc "qpdf" ["--linearize", "temp.pdf", outFile stamp])
              { std_in = NoStream
              , std_out = NoStream
              , std_err = UseHandle logH
              }
      -- assume that only one page was scanned.  Not clear how we
      -- can avoid this assumption when producing a PNG
      PNG -> void $ createProcessWait_ "convert"
        (proc "convert" ["page1" <.> "tiff", outFile stamp])
          { std_in = NoStream
          , std_out = NoStream
          , std_err = UseHandle logH
          }
    hClose logH
    -- clean up the log file if it's empty
    finalLog <- readFile (logFile stamp)
    when (null finalLog) $ removeFile (logFile stamp)
  where
      logFile stamp = (st^.stOutdir) </> stamp <.> "log"
      outFile stamp = (st^.stOutdir) </> stamp <.> outExt
      outExt = case st^.stOutFormat of
        PDF -> "pdf"
        PNG -> "png"
      allPages = map (\n -> "page" ++ show n <.> "tiff") [1..(getLatestPage st)]
      metadata posix = let date = formatTime
                                  defaultTimeLocale
                                  (iso8601DateFormat Nothing)
                                  (posixSecondsToUTCTime posix)
                       in unlines [ "InfoBegin"
                                  , "InfoKey: Title"
                                  , "InfoValue: scan of " ++ date
                                  , "InfoBegin"
                                  , "InfoKey: Author"
                                  , "InfoValue: spw"
                                  ]

makeInitialState :: IO St
makeInitialState = do
    home <- getHomeDirectory
    papersize <- init <$> readFile "/etc/papersize"
    let paper = if papersize == "letter" then Letter else A4
    return St
        { _stScanSess     = Nothing
        , _stOCR          = True
        , _stColour       = Greyscale
        , _stPaper        = paper
        , _stDefaultPaper = paper
        , _stDPI          = 300
        , _stOutFormat    = PDF
        , _stOutdir       = home </> "tmp"
        }

scanPage :: St -> FilePath -> IO Bool
scanPage st dir = withFile outF WriteMode $ \outH -> do
    exit <- createProcessWait_ "scanimage" (proc "scanimage" (scanimageArgs st))
        { std_in = NoStream
        , std_out = UseHandle outH
        , std_err = Inherit     -- let the user see progress bar
        }
    case exit of
      ExitSuccess -> return True
      ExitFailure c -> do
          putStrLn $ "scanimage exited with exit code " ++ show c ++ "!"
          putStrLn "This might because sscan tried to use an option that your"
          putStrLn "scanner does not support.  Please try another preset."
          putStrLn "Press any key to abort this scanning session..."
          void getChar
          return False
  where
      outF = dir </> "page" ++ (show $ getLatestPage st + 1) <.> "tiff"

scanimageArgs :: St -> [String]
scanimageArgs st =
    [ "-vp"
    , "--format=tiff"
    , "--resolution=" ++ show (st^.stDPI)
    , "--mode=" ++ case st^.stColour of
        Colour    -> "Color"
        Greyscale -> "Gray"
        Lineart   -> "Lineart"
    ] ++ case st^.stPaper of
           Auto -> ["--swcrop=yes"]
           _ -> [ "-x"
                , show $ case st^.stPaper of
                    A4     -> 210   :: Double
                    Letter -> 215.9 :: Double
                    Photo  -> 150   :: Double
                    Auto   -> 1     :: Double
                , "-y"
                , show $ case st^.stPaper of
                    A4     -> 297   :: Double
                    Letter -> 279.4 :: Double
                    Photo  -> 100   :: Double
                    Auto   -> 1     :: Double
                ]
-- other device-specific scanimage options the old script supported:
-- --swdespeck; --color-filter; --depth

-- TODO include usbreset.c as a cbit, and use FFI to optionally reset
-- the scanner's USB connection after a scan -- this is needed on some
-- scanners that I occasionally use

processCommand :: St -> IO ()
processCommand st = case st^.stScanSess of
  Nothing -> return ()          -- quit sscan
  Just (ScanSess command _ maybeDir) -> case maybeDir of
    Nothing -> withSystemTempDirectory "sscan" $ \dir ->
      processCommand (setScanSessDir dir st)
    Just dir -> case command of
      Abort -> newSession
      NextPage -> scanPage st dir >>= \scanned ->
        if scanned
        then presentUI (incrementPages st)
        else newSession
      FinalPage -> scanPage st dir
                >> finaliseSession (incrementPages st) dir
      Finalise -> finaliseSession st dir
  where
    newSession = presentUI $ resetScanSess st
    finaliseSession st' dir =
        concurrently_ (processScanSessDir st' dir) newSession

presentUI    :: St -> IO ()
presentUI st = runTheApp st >>= processCommand

main :: IO ()
main = makeInitialState >>= presentUI

-- | Create a process, wait for it to finish, don't close any
-- handles used by the CreateProcess record
createProcessWait_ :: String -> CreateProcess -> IO ExitCode
createProcessWait_ s c = do
    (_, _, _, p) <- createProcess_ s c
    waitForProcess p
