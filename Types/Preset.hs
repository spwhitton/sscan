module Types.Preset where

import qualified Data.Text   as T

import           Types.State

type PresetToggleKey = Char
type PresetDesc = T.Text
type PresetPreset = St -> St

data Preset = Preset PresetToggleKey PresetDesc PresetPreset
