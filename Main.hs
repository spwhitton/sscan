{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad        (void)
import           Data.Monoid
import qualified Graphics.Vty         as V
import           Lens.Micro           ((&), (.~), (^.))

import           Brick.AttrMap
import           Brick.Main
import           Brick.Markup         (markup, (@?))
import           Brick.Types
import           Brick.Util           (fg, on)
import           Brick.Widgets.Border as B
import           Brick.Widgets.Center as C
import           Brick.Widgets.Core
import           Data.Text.Markup     ((@@))

import           Types

drawUI :: St -> [Widget ()]
drawUI st = [ui]
  where
    ui = vBox [ hBorderWithLabel (str "[ Status ]")
              , C.center $ status
              , hBorderWithLabel (str "[ Current settings ]")
              , padAll 1 $ C.center $ settings
              , hBorderWithLabel (str "[ Presets ]")
              , padAll 1 $ C.center $ presets
              , hBorderWithLabel (str "[ Actions ]")
              , padAll 1 $ C.center $ actions
              ]
    status = str "Ready to scan first page"
    settings = vBox [ str $ "run OCRmyPDF: " ++ if st^.stOCR then "yes" else "no"
                    , str $ "colour data: " ++ (show $ st^.stColour)
                    , str $ "page size: " ++ (show $ st^.stPaper)
                    , str $ "DPI: " ++ (show $ st^.stDPI)
                    ]
    presets = vBox [ markup $ ("h:" @@ (V.withStyle V.currentAttr V.bold))
                     <> (" handwritten notes" @@ fg V.white)
                   ]
    actions = str "actions"

appEvent :: St -> BrickEvent () e -> EventM () (Next St)
appEvent st (VtyEvent e) =
    case e of
      -- settings toggles
      V.EvKey (V.KChar 'o') [] -> continue $ st & stOCR .~ (not $ st^.stOCR)
      V.EvKey (V.KChar 'c') [] -> continue $
          st & stColour .~ (cycleColour $ st^.stColour)
      V.EvKey (V.KChar 'p') [] -> continue $
          st & stPaper .~ (cyclePaper $ st^.stPaper)

      -- presets: set several settings toggles at once
      V.EvKey (V.KChar 'h') [] -> continue $ st
          { _stOCR = False
          , _stColour = Greyscale
          , _stDPI = 75
          }

      -- actions
      V.EvKey (V.KChar 'q') [] -> halt st

      _ -> continue st
appEvent st _ = continue st

initialState :: St
initialState =
    St { _stScanningSession = Nothing
       , _stOCR = True
       , _stColour = Greyscale
       , _stPaper = A4
       , _stDPI = 300
       , _stOutdir = ""
       }

theApp :: App St e ()
theApp =
    App { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = appEvent
        , appStartEvent = return
        , appAttrMap = const $ attrMap V.defAttr []
        }

main = do
    void $ defaultMain theApp initialState
