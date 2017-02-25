{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Monad        (void)
import           Data.Monoid
import qualified Graphics.Vty         as V
import           Lens.Micro           ((&), (.~), (^.))
import           Lens.Micro.TH        (makeLenses)

import           Brick.AttrMap
import           Brick.Main
import           Brick.Types
import           Brick.Widgets.Border as B
import           Brick.Widgets.Center as C
import           Brick.Widgets.Core

data St =
    St { _stExternalInput :: String
       }

makeLenses ''St

drawUI :: St -> [Widget ()]
drawUI st = [ui]
  where
    ui = vBox [ hBorderWithLabel (str "[ Status ]")
              , padAll 1 $ C.center $ status
              , hBorderWithLabel (str "[ Current settings ]")
              , padAll 1 $ C.center $ settings
              , hBorderWithLabel (str "[ Presets ]")
              , padAll 1 $ C.center $ presets
              , hBorderWithLabel (str "[ Actions ]")
              , padAll 1 $ C.center $ actions
              ]
    status = str "Ready to scan first page"
    settings = str "settings"
    presets = str "presets"
    actions = str "actions"

initialState :: St
initialState =
    St { _stExternalInput = ""
       }

theApp :: App St e ()
theApp =
    App { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = resizeOrQuit
        , appStartEvent = return
        , appAttrMap = const $ attrMap V.defAttr []
        }

main = do
    void $ defaultMain theApp initialState
