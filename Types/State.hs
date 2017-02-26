{-# LANGUAGE TemplateHaskell #-}

module Types.State where

import           Lens.Micro.TH        (makeLenses)

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

-- | Application state
data St =
    St { _stScanningSession :: Maybe FilePath -- ^ if a session is in
                                              -- progress, accmulate
                                              -- scans in this dir
       , _stOCR             :: Bool -- ^ whether to use OCRmyPDF
       , _stColour          :: Colour
       , _stPaper           :: Paper -- ^ currently selected paper size
       , _stDefaultPaper    :: Paper -- ^ locale's default paper size
       , _stDPI             :: DPI
       , _stOutFormat       :: OutputFormat
       , _stOutdir          :: FilePath -- ^ where to save final PDFs
       }

-- other device-specific scanimage options we could support: --swdespeck; --color-filter; --depth

makeLenses ''St
