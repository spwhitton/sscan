{-# LANGUAGE OverloadedStrings #-}

module Presets (presets, lookupPreset) where

import Data.List (find)

import Types.Preset
import Types.State

presets :: [Preset]
presets = [ Preset 'h' "handwritten notes" handwrittenNotes
          , Preset 't' "typewritten docs" typewrittenDocs
          ]

lookupPreset :: Char -> Maybe Preset
lookupPreset c = find (\(Preset k _ _) -> c == k) presets

handwrittenNotes = \st -> st
    { _stOCR     = False
    , _stColour  = Greyscale
    , _stDPI     = 75
    }

typewrittenDocs = \st -> st
    { _stOCR    = True
    , _stColour = Greyscale
    , _stDPI    = 300
    }
