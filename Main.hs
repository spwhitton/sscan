{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad          (void)
import           Data.Monoid
import qualified Data.Text              as T
import qualified Graphics.Vty           as V
import           Lens.Micro             ((&), (.~), (^.))
import           System.Directory       (getHomeDirectory)
import           System.FilePath        ((</>))

import           Brick.AttrMap
import           Brick.Main
import           Brick.Markup           (markup, (@?))
import           Brick.Types
import           Brick.Util             (fg, on)
import           Brick.Widgets.Border   as B
import           Brick.Widgets.Center   as C
import           Brick.Widgets.Core
import           Data.Text.Markup       ((@@))

import           Brick.Widgets.DefnList
import           Presets
import           Types.Preset
import           Types.State

drawUI :: St -> [Widget ()]
drawUI st = [ui]
  where
    ui = vBox [ hBorderWithLabel (str "[ Status ]")
              , vLimit 3 $ C.center $ status
              , hBorderWithLabel (str "[ Current settings ]")
              , padAll 1 $ C.center $ settingsBox
              , hBorderWithLabel (str "[ Presets ]")
              , padAll 1 $ C.center $ presetsBox
              , hBorderWithLabel (str "[ Actions ]")
              , padAll 1 $ C.center $ actionsBox
              ]
    status = str "Ready to scan first page"
    settingsBox = defnList
        [ ("run OCRmyPDF", if st^.stOCR then "yes" else "no")
        , ("colour data",   show $ st^.stColour)
        , ("page size",     show $ st^.stPaper)
        , ("DPI",           show $ st^.stDPI)
        , ("output format", show $ st^.stOutFormat)
        , ("output dir",    st^.stOutdir)
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
  _                   -> continue st

appEvent :: St -> BrickEvent () e -> EventM () (Next St)
appEvent st (VtyEvent e) =
    case e of
      V.EvKey (V.KChar c) [] -> handleHotKey st c
      _                      -> continue st
appEvent st _ = continue st

theApp :: App St e ()
theApp =
    App { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = appEvent
        , appStartEvent = return
        , appAttrMap = const $ attrMap V.defAttr []
        }

main = do
    home <- getHomeDirectory
    papersize <- init <$> readFile "/etc/papersize"
    let paper = if papersize == "letter" then Letter else A4
        initialState = St
            { _stScanningSession = Nothing
            , _stOCR             = True
            , _stColour          = Greyscale
            , _stPaper           = paper
            , _stDefaultPaper    = paper
            , _stDPI             = 300
            , _stOutFormat    = PDF
            , _stOutdir          = home </> "tmp"
            }
    void $ defaultMain theApp initialState
