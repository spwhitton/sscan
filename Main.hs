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
import           Data.Maybe             (isJust)
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
              , vLimit 5 $ C.center $ actionsBox
              ]
    status = str "Ready to scan first page"
    settingsBox = defnList AlignRight Nothing
        [ ("run OCRmyPDF", if st^.stOCR then "yes" else "no")
        , ("colour data",   show $ st^.stColour)
        , ("page size",     show $ st^.stPaper)
        , ("DPI",           show $ st^.stDPI)
        , ("output format", show $ st^.stOutFormat)
        , ("output dir",    st^.stOutdir)
        ]
    presetsBox = defnList AlignLeft
        (Just $ V.withStyle V.currentAttr V.bold)
        (map (\(Preset k desc _) -> ([k], desc)) presets)
    actionsBox = defnList AlignLeft
        (Just $ V.withStyle V.currentAttr V.bold) $
        (if st^.stOutFormat == PDF
         then (if isJust $ st^.stScanningSession
               then [ ("SPC", "scan next page")
                    , ("RET", "scan final page")
                    , ("q", "declare last scanned page was the final page")
                    ]
               else [ ("SPC", "scan first page of multi-page document")
                    , ("RET", "scan single page document")
                    , ("q", "quit sscan")
                    ]
              )
         else [ ("SPC", "scan page")
              , ("q", "quit sscan")
              ]
        )

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
            , _stOutFormat       = PDF
            , _stOutdir          = home </> "tmp"
            }
    void $ defaultMain theApp initialState
