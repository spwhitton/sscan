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

import           Control.Concurrent (forkFinally)
import           Control.Monad      (void)
import           Lens.Micro         ((&), (.~), (^.))
import           System.Directory   (getHomeDirectory, removeDirectoryRecursive)
import           System.FilePath    ((</>))
import           System.IO.Temp     (createTempDirectory)

import           Types.State
import           UI

-- processScanSessDir :: ScanSess -> IO ()
-- processScanSessDir ss@(ScanSess d n) =
--     -- rather than do any error handling here, we write a logfile to
--     -- the outdir for user inspection
--     void $ forkFinally process $ \_ -> nukeScanSessDir ss
--   where
--     -- run OCRmyPDF, pdftk etc., and if any process existed non-zero,
--     -- record to a log file in the outdir
--     process = undefined

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
        , _stCommand      = Quit
        }

uiLoop :: St -> IO ()
uiLoop st = runTheApp st >>= \newSt -> do
    undefined

main = makeInitialState >>= uiLoop

-- TODO scanning should happen in main.  We use withTempDir to setup a
-- session, and then fire up brick again.  add additional state
-- element that is the user's chosen action.  i.e. don't use
-- suspendAndResume which is for getting user input, not running the
-- result of the UI.  Move the whole UI to a module
