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

import           Control.Concurrent    (forkIO)
import           Control.Monad         (void, when)
import           Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import           Data.Time.Format      (defaultTimeLocale, formatTime,
                                        iso8601DateFormat)
import           Lens.Micro            ((^.))
import           System.Directory      (getHomeDirectory, renamePath,
                                        withCurrentDirectory)
import           System.Exit           (ExitCode (..))
import           System.FilePath       ((<.>), (</>))
import           System.IO             (IOMode (WriteMode), hClose, openFile,
                                        withFile)
import           System.IO.Temp        (withSystemTempDirectory)
import           System.Process

import           Types.State
import           UI

processScanSessDir :: St -> FilePath -> IO ()
processScanSessDir st dir = withCurrentDirectory dir $ do
    posix <- getPOSIXTime
    let stamp = show . round $ posix
    logH <- openFile (logFile stamp) WriteMode -- TODO maybe AppendMode?
    outH <- openFile (outFile stamp) WriteMode
    case st^.stOutFormat of
      PDF -> do
          -- 1. convert tiff->PDF
          createProcessWait_ "convert"
              (proc "convert" (allPages ++ ["temp.pdf"]))
          -- 2. set metadata with pdftk
          renamePath "temp.pdf" "temp2.pdf"
          writeFile "metadata" (metadata posix)
          createProcessWait_ "pdftk"
              (proc "pdftk" ["temp2.pdf", "update_info", "metadata", "temp.pdf"])
              { std_in = NoStream
              , std_out = NoStream
              , std_err = UseHandle logH
              }
          -- 3. maybe ocrmypdf
          when (st^.stOCR) $ do
              renamePath "temp.pdf" "temp2.pdf"
              void $ createProcessWait_ "OCRmyPDF"
                  (proc "ocrmypdf" ["-c", "-i", "-r", "temp2.pdf", "temp.pdf"])
                  { std_in = NoStream
                  , std_out = NoStream
                  , std_err = UseHandle logH
                  }
          -- 4. qpdf (ocrmypdf invokes qpdf but it doesn't use
          -- --linearize, which shrinks the PDF, often substantially)
          createProcessWait_ "qpdf" (proc "qpdf" ["--linearize", "temp.pdf"])
              { std_in = NoStream
              , std_out = UseHandle outH
              , std_err = UseHandle logH
              }
      -- assume that only one page was scanned.  Not clear how we
      -- can avoid this assumption when producing a PNG
      PNG -> createProcessWait_ "convert"
        (proc "convert" ["page1" <.> "tiff", "png:-"])
          { std_in = NoStream
          , std_out = UseHandle outH
          , std_err = UseHandle logH
          }
    hClose outH
    hClose logH
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
                       in unlines [ "InfoKey: Title"
                                  , "InfoValue: scan of " ++ date
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
                    A4     -> 210
                    Letter -> 215.9
                    Photo  -> 150
                , "-y"
                , show $ case st^.stPaper of
                    A4     -> 297
                    Letter -> 279.4
                    Photo  -> 100
                ]
-- other device-specific scanimage options the old script supported:
-- --swdespeck; --color-filter; --depth

-- TODO include usbreset.c as a cbit, and use FFI to optionally reset
-- the scanner's USB connection after a scan -- this is needed on some
-- scanners that I occasionally use

processCommand :: St -> IO ()
processCommand st = case st^.stScanSess of
  Nothing -> return ()          -- quit sscan
  Just (ScanSess command pages maybeDir) -> case maybeDir of
    Nothing -> withSystemTempDirectory "sscan" $ \dir ->
      processCommand (setScanSessDir dir st)
    Just dir -> case command of
      Abort -> newSession
      NextPage -> scanPage st dir >>= \scanned ->
        if scanned
        then presentUI (incrementPages st)
        else newSession
      FinalPage -> scanPage st dir
                >> finaliseSession (incrementPages st) dir >> newSession
      Finalise -> finaliseSession st dir >> newSession
  where
    newSession = presentUI $ resetScanSess st
    finaliseSession st dir = forkIO $ processScanSessDir st dir

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
