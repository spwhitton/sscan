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

-- | Application state
data St =
    St { _stScanningSession :: Maybe FilePath -- ^ if a session is in
                                              -- progress, accmulate
                                              -- scans in this dir
       , _stOCR             :: Bool -- ^ whether to use OCRmyPDF
       , _stColour          :: Colour
       , _stPaper           :: Paper
       , _stDPI             :: DPI
       , _stOutdir          :: FilePath -- ^ where to save final PDFs
       }

makeLenses ''St
