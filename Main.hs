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
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Lens.Micro            ((&), (.~), (^.))
import           System.Directory      (getHomeDirectory,
                                        removeDirectoryRecursive, renamePath,
                                        withCurrentDirectory)
import           System.FilePath       ((<.>), (</>))
import           System.IO             (IOMode (WriteMode), hClose, openFile)
import           System.IO.Temp        (withSystemTempDirectory)
import           System.Process

import           Types.State
import           UI

processScanSessDir :: St -> FilePath -> IO ()
processScanSessDir st dir = withCurrentDirectory dir $ do
    stamp <- round <$> getPOSIXTime
    logH <- openFile (logFile stamp) WriteMode
    outH <- openFile (outFile stamp) WriteMode
    case st^.stOutFormat of
      PDF -> do
          -- 1. convert tiff->PDF
          createProcessWait_ "convert"
              (proc "convert" (allPages ++ ["temp.pdf"]))
          -- 2. set metadata with pdftk
          renamePath "temp.pdf" "temp2.pdf"
          writeFile "metadata" metadata
          createProcessWait_ "pdftk"
              (proc "pdftk" ["temp2.pdf", "update_info", "metadata", "temp.pdf"])
              { std_in = NoStream
              , std_out = NoStream
              , std_err = UseHandle logH
              }
          -- 3. maybe ocrmypdf
          when (st^.stOCR) $ renamePath "temp.pdf" "temp2.pdf"
              >> createProcessWait_ "OCRmyPDF"
              (proc "ocrmypdf" ["-c", "-i", "-r", "temp2.pdf", "temp.pdf"])
              { std_in = NoStream
              , std_out = NoStream
              , std_err = UseHandle logH
              }
          -- 4. qpdf (ocrmypdf might invoke this; do it again as I
          -- think that OCRmyPDF isn't using its --linearize option,
          -- which shrinks the PDF)
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
      logFile stamp = (st^.stOutdir) </> show stamp <.> "log"
      outFile stamp = (st^.stOutdir) </> show stamp <.> outExt
      outExt = case st^.stOutFormat of
        PDF -> "pdf"
        PNG -> "png"
      allPages = map (\n -> "page" ++ show n <.> "tiff") [1..(getLatestPage st)]
      metadata = undefined

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

scanPage :: St -> FilePath -> IO ()
scanPage st dir = do
    outH <- openFile outF WriteMode
    -- TODO if scanimage exists non-zero, inform the user that we will
    -- abort the scan session, pause for them to read the output, and then
    -- abort the scan session
    createProcessWait_ "scanimage" (proc "scanimage" (scanimageArgs st))
        { std_in = NoStream
        , std_out = UseHandle outH
        , std_err = Inherit     -- let the user see progress bar
        }
    hClose outH
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
    , "-x"
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

processCommand :: St -> IO ()
processCommand st = case st^.stScanSess of
  Nothing -> return ()          -- quit sscan
  Just (ScanSess command pages maybeDir) -> case maybeDir of
    Nothing -> withSystemTempDirectory "sscan" $ \dir ->
      processCommand (setScanSessDir dir st)
    Just dir -> case command of
      Abort -> newSession
      NextPage -> scanPage st dir >> presentUI (incrementPages st)
      FinalPage -> scanPage st dir
                >> finaliseSession (incrementPages st) dir >> newSession
      Finalise -> finaliseSession st dir >> newSession
  where
    newSession = presentUI $ resetScanSess st
    finaliseSession st dir = forkIO $ processScanSessDir st dir

presentUI    :: St -> IO ()
presentUI st = runTheApp st >>= processCommand

main = makeInitialState >>= presentUI

-- | Create a process, wait for it to finish, don't close any
-- handles used by the CreateProcess record
createProcessWait_ :: String -> CreateProcess -> IO ()
createProcessWait_ s c = do
    (_, _, _, p) <- createProcess_ s c
    void $ waitForProcess p

-- TODO scanning should happen in main.  We use withTempDir to setup a
-- session, and then fire up brick again.  add additional state
-- element that is the user's chosen action.  i.e. don't use
-- suspendAndResume which is for getting user input, not running the
-- result of the UI.  Move the whole UI to a module
