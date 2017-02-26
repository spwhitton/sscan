module Types.Preset where

import qualified Data.Text   as T

import           Types.State

type PresetToggleKey = Char
type PresetDesc = String
type PresetPreset = St -> St

data Preset = Preset PresetToggleKey PresetDesc PresetPreset
