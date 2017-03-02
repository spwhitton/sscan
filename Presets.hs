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

module Presets (presets, lookupPreset) where

import           Data.List    (find)
import           Lens.Micro   ((^.))

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

handwrittenNotes :: St -> St
handwrittenNotes = \st -> st
    { _stOCR     = False
    , _stColour  = Greyscale
    , _stDPI     = 75
    , _stPaper   = st^.stDefaultPaper
    , _stOutFormat = PDF
    }

typewrittenDocs :: St -> St
typewrittenDocs = \st -> st
    { _stOCR    = True
    , _stColour = Greyscale
    , _stDPI    = 300
    , _stPaper  = st^.stDefaultPaper
    , _stOutFormat = PDF
    }

colourPDF :: St -> St
colourPDF = \st -> st
    { _stOCR = False
    , _stColour = Colour
    , _stDPI = 300
    , _stPaper = st^.stDefaultPaper
    , _stOutFormat = PDF
    }

colourPNG :: St -> St
colourPNG = \st -> st
    { _stOCR = False
    , _stColour = Colour
    , _stDPI = 300
    , _stPaper = st^.stDefaultPaper
    , _stOutFormat = PNG
    }

colourCroppedPNG :: St -> St
colourCroppedPNG = \st -> st
    { _stOCR = False
    , _stColour = Colour
    , _stDPI = 300
    , _stPaper = Auto
    , _stOutFormat = PNG
    }

bnwPDF :: St -> St
bnwPDF = \st -> st
    { _stOCR = False
    , _stColour = Lineart
    , _stDPI = 150
    , _stPaper = st^.stDefaultPaper
    , _stOutFormat = PDF
    }

photo :: St -> St
photo = \st -> st
    { _stOCR = False
    , _stColour = Colour
    , _stDPI = 600
    , _stPaper = Photo
    , _stOutFormat = PNG
    }
