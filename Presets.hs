{-# LANGUAGE OverloadedStrings #-}

module Presets (presets, lookupPreset) where

import           Data.List    (find)
import           Lens.Micro   ((&), (.~), (^.))

import           Types.Preset
import           Types.State

presets :: [Preset]
presets = [ Preset 'H' "handwritten notes" handwrittenNotes
          , Preset 'T' "typewritten docs" typewrittenDocs
          , Preset 'C' "standard DPI colour PDF, full page" colourPDF
          , Preset 'I' "standard DPI colour PNG, full page" colourPNG
          , Preset 'A' "standard DPI colour PNG, autocropped" colourCroppedPNG
          , Preset 'B' "black and white PDF, full page" bnwPDF
          , Preset 'P' "high DPI 6x4 photo" photo
          ]

lookupPreset :: Char -> Maybe Preset
lookupPreset c = find (\(Preset k _ _) -> c == k) presets

handwrittenNotes = \st -> st
    { _stOCR     = False
    , _stColour  = Greyscale
    , _stDPI     = 75
    , _stPaper   = st^.stDefaultPaper
    , _stOutFormat = PDF
    }

typewrittenDocs = \st -> st
    { _stOCR    = True
    , _stColour = Greyscale
    , _stDPI    = 300
    , _stPaper  = st^.stDefaultPaper
    , _stOutFormat = PDF
    }

colourPDF = \st -> st
    { _stOCR = False
    , _stColour = Colour
    , _stDPI = 300
    , _stPaper = st^.stDefaultPaper
    , _stOutFormat = PDF
    }

colourPNG = \st -> st
    { _stOCR = False
    , _stColour = Colour
    , _stDPI = 300
    , _stPaper = st^.stDefaultPaper
    , _stOutFormat = PNG
    }

colourCroppedPNG = \st -> st
    { _stOCR = False
    , _stColour = Colour
    , _stDPI = 300
    , _stPaper = Auto
    , _stOutFormat = PNG
    }

bnwPDF = \st -> st
    { _stOCR = False
    , _stColour = Lineart
    , _stDPI = 150
    , _stPaper = st^.stDefaultPaper
    , _stOutFormat = PDF
    }

photo = \st -> st
    { _stOCR = False
    , _stColour = Colour
    , _stDPI = 600
    , _stPaper = Photo
    , _stOutFormat = PNG
    }
