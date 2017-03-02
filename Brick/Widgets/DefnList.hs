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

module Brick.Widgets.DefnList (defnList, Align(..)) where

import           Data.Monoid
import qualified Data.Text          as T
import qualified Graphics.Vty       as V

import           Brick.Markup       (markup)
import           Brick.Types
import           Brick.Widgets.Core
import           Data.Text.Markup   ((@@))

data Align = AlignLeft | AlignRight
    deriving (Eq)

defnList :: Align -> Maybe V.Attr -> [(String, String)] -> Widget ()
defnList align attr defns = vBox $ line <$> defns
  where
    line (label, content) = markup $
        (T.pack label @@ labelAttr) <> (T.pack sep @@ V.defAttr)
        <> (T.pack (gap label content) @@ V.defAttr)
        <> (T.pack content @@ V.defAttr)

    gap a b
        | align == AlignRight =
              take (maxWidth - length a - length b - length sep) $
              repeat ' '
        | align == AlignLeft =
              take (maxLabelWidth - length a) $
              repeat ' '

    maxWidth = maximum $
        map (\(x,y) -> length sep + length x + length y) defns
    maxLabelWidth = maximum $ map (\(x,_) -> length x) defns
    sep = if align == AlignRight then ":   " else ": "

    labelAttr = maybe V.defAttr id attr
