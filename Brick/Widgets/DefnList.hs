module Brick.Widgets.DefnList (defnList) where

import           Brick.Types
import           Brick.Widgets.Core

defnList :: [(String, String)] -> Widget ()
defnList defns = vBox $ line <$> defns
  where
    line (label, content) = str $
        label ++ sep ++ (gap label content) ++ content
    gap a b = take (maxWidth - length a - length b - length sep) $ repeat ' '
    maxWidth = maximum $ map (\(x,y) -> length sep + length x + length y) defns
    sep = ":   "
