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

module Types.Preset where

import           Types.State

type PresetToggleKey = Char
type PresetDesc      = String
type PresetPreset    = St -> St

data Preset = Preset PresetToggleKey PresetDesc PresetPreset
