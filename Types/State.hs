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

{-# LANGUAGE TemplateHaskell #-}

module Types.State where

import           Data.Maybe
import           Lens.Micro    ((&), (.~), (^.))
import           Lens.Micro.TH (makeLenses)

-- | Whether to do colour, grey or b&w scans
data Colour = Lineart | Greyscale | Colour
    deriving (Eq, Show)

cycleColour :: Colour -> Colour
cycleColour Lineart   = Greyscale
cycleColour Greyscale = Colour
cycleColour Colour    = Lineart

-- | Paper size to scan (determines both scanning area and PDF page
-- size)
data Paper = A4 | Letter | Photo | Auto
    deriving (Eq, Show)

cyclePaper :: Paper -> Paper
cyclePaper A4     = Letter
cyclePaper Letter = Photo
cyclePaper Photo  = Auto
cyclePaper Auto   = A4

-- | DPI to scan
type DPI = Int

-- | Output format
data OutputFormat = PDF | PNG
    deriving (Eq, Show)

-- | An active multi-page scanning session
data ScanSess = ScanSess
    FilePath                    -- ^ session's tmpdir
    Int                         -- ^ total pages scanner thus far

-- | An sscan operation selected by the user in the UI
data Command
    = NextPage                  -- ^ scan first page or another page
    | FinalPage                 -- ^ scan final page (might be the first page)
    | Finalise                  -- ^ make the previous scanned page the final page of the document
    | Abort                     -- ^ abandon the current scan session
    | Quit                      -- ^ quit sscan

-- | Application state
data St =
    St { _stScanSess     :: Maybe ScanSess
       , _stOCR          :: Bool -- ^ whether to use OCRmyPDF
       , _stColour       :: Colour
       , _stPaper        :: Paper -- ^ currently selected paper size
       , _stDefaultPaper :: Paper -- ^ locale's default paper size
       , _stDPI          :: DPI
       , _stOutFormat    :: OutputFormat
       , _stOutdir       :: FilePath -- ^ where to save final PDFs
       , _stCommand      :: Command
       }
-- other device-specific scanimage options the old script supported: --swdespeck; --color-filter; --depth

makeLenses ''St

ifScanSess :: St -> a -> a -> a
ifScanSess st a b = maybe b (const a) (st^.stScanSess)

-- | Update a state when there is no scanning session in progress (the
-- state should not be changed when some pages have already been
-- scanned)
updateSt :: St -> (St -> St) -> St
updateSt st f = ifScanSess st st (f st)

resetScanSess :: St -> St
resetScanSess st = st & stScanSess .~ Nothing
