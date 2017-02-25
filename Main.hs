{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad        (void)
import           Data.Monoid
import qualified Graphics.Vty         as V
import           Lens.Micro           ((&), (.~), (^.))
import qualified Data.Text as T

import           Brick.AttrMap
import           Brick.Main
import           Brick.Markup         (markup, (@?))
import           Brick.Types
import           Brick.Util           (fg, on)
import           Brick.Widgets.Border as B
import           Brick.Widgets.Center as C
import           Brick.Widgets.Core
import           Data.Text.Markup     ((@@))

import           Presets
import           Types.State
import           Types.Preset

drawUI :: St -> [Widget ()]
drawUI st = [ui]
  where
    ui = vBox [ hBorderWithLabel (str "[ Status ]")
              , C.center $ status
              , hBorderWithLabel (str "[ Current settings ]")
              , padAll 1 $ C.center $ settingsBox
              , hBorderWithLabel (str "[ Presets ]")
              , padAll 1 $ C.center $ presetsBox
              , hBorderWithLabel (str "[ Actions ]")
              , padAll 1 $ C.center $ actionsBox
              ]
    status = str "Ready to scan first page"
    settingsBox = vBox [ str $ "run OCRmyPDF: "
                         <> if st^.stOCR then "yes" else "no"
                       , str $ "colour data: " ++ (show $ st^.stColour)
                       , str $ "page size: " ++ (show $ st^.stPaper)
                       , str $ "DPI: " ++ (show $ st^.stDPI)
                       ]
    presetsBox = vBox $
        (\(Preset k desc _) ->
            markup $
            (((T.pack [k]) <> ": ") @@ (V.withStyle V.currentAttr V.bold))
            <> (desc @@ fg V.white))
        <$> presets
    actionsBox = str "actions"

handleHotKey :: St -> Char -> EventM () (Next St)
handleHotKey st 'q' = halt st
handleHotKey st 'o' = continue $ st & stOCR .~ (not $ st^.stOCR)
handleHotKey st 'c' = continue $
    st & stColour .~ (cycleColour $ st^.stColour)
handleHotKey st 'p' = continue $
    st & stPaper .~ (cyclePaper $ st^.stPaper)
handleHotKey st c = case lookupPreset c of
  Just (Preset _ _ f) -> continue $ f st
  _ -> continue st

appEvent :: St -> BrickEvent () e -> EventM () (Next St)
appEvent st (VtyEvent e) =
    case e of
      V.EvKey (V.KChar c) [] -> handleHotKey st c
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
