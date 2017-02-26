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
    maxLabelWidth = maximum $ map (\(x,y) -> length x) defns
    sep = if align == AlignRight then ":   " else ": "

    labelAttr = maybe V.defAttr id attr
