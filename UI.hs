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

module UI (runTheApp) where

import qualified Graphics.Vty           as V
import           Lens.Micro             ((&), (.~), (^.))

import           Brick.AttrMap
import           Brick.Main
import           Brick.Types
import           Brick.Widgets.Border   as B
import           Brick.Widgets.Center   as C
import           Brick.Widgets.Core

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
              , vLimit 8 $ C.center $ settingsBox
              , hBorderWithLabel (str "[ Presets ]")
              , vLimit (2 + length presets) $ C.center $ presetsBox
              , hBorderWithLabel (str "[ Actions ]")
              , vLimit 6 $ C.center $ actionsBox
              ]
    status = str $ maybe
        "Ready to scan first page"
        (\(ScanSess _ p _) -> "Scanned " ++ show p ++ " pages")
        (st^.stScanSess)
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
         then (ifScanSess st
                [ ("SPC", "scan next page")
                , ("RET", "scan final page")
                , ("q", "declare most recent scanned page was the final page")
                , ("ESC", "abort/restart scanning this document")
                ]
                [ ("SPC", "scan first page of multi-page document")
                , ("RET", "scan single page document")
                , ("q", "quit sscan")
                ]
              )
         else [ ("SPC|RET", "scan page")
              , ("q", "quit sscan")
              ]
        )

handleQ :: St -> EventM () (Next St)
handleQ st = halt $ ifScanSess st
    (setScanSessCommand Finalise st)
    (resetScanSess st)          -- misnomer: this causes sscan to exit

handleRET :: St -> EventM () (Next St)
handleRET st = halt $ setScanSessCommand FinalPage st

handleSPC :: St -> EventM () (Next St)
handleSPC st = halt $ setScanSessCommand
    (case st^.stOutFormat of
       PDF -> NextPage
       PNG -> FinalPage)
    st

handleESC :: St -> EventM () (Next St)
handleESC st = ifScanSess st
    (halt $ setScanSessCommand Abort st)
    (continue st)

handleHotKey :: St -> Char -> EventM () (Next St)
handleHotKey st 'q' = handleQ st
handleHotKey st ' ' = handleSPC st
handleHotKey st 'o' = continue $ updateSt st
    (\s -> s & stOCR .~ (not $ st^.stOCR))
handleHotKey st 'c' = continue $ updateSt st
    (\s -> s & stColour .~ (cycleColour $ st^.stColour))
handleHotKey st 'p' = continue $ updateSt st
    (\s -> s & stPaper .~ (cyclePaper $ st^.stPaper))
handleHotKey st c = continue $ updateSt st $
    case lookupPreset c of
      Just (Preset _ _ f) -> f
      _                   -> id

appEvent :: St -> BrickEvent () e -> EventM () (Next St)
appEvent st (VtyEvent e) =
    case e of
      V.EvKey (V.KEnter) []  -> handleRET st
      V.EvKey (V.KEsc) []    -> handleESC st
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

runTheApp :: St -> IO St
runTheApp = defaultMain theApp
