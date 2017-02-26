{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent     (forkFinally)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid
import qualified Data.Text              as T
import qualified Graphics.Vty           as V
import           Lens.Micro             ((&), (.~), (^.))
import           System.Directory       (getHomeDirectory,
                                         getTemporaryDirectory,
                                         removeDirectoryRecursive)
import           System.FilePath        ((</>))
import           System.IO.Temp         (createTempDirectory)

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
              , vLimit 6 $ C.center $ actionsBox
              ]
    status = str $ case st^.stPageCount of
      Just n  -> "Scanned " ++ show n ++ " pages"
      Nothing -> "Ready to scan first page"
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
                , ("q", "declare last scanned page was the final page")
                , ("ESC", "abort/restart scanning this document")
                ]
                [ ("SPC", "scan first page of multi-page document")
                , ("RET", "scan single page document")
                , ("q", "quit sscan")
                ]
              )
         else [ ("SPC", "scan page")
              , ("q", "quit sscan")
              ]
        )

handleQ :: St -> EventM () (Next St)
handleQ st = ifScanSess st
    ((liftIO . finishScanSess) st >>= continue)
    (halt st)

handleRET :: St -> EventM () (Next St)
handleRET st = suspendAndResume $ ifScanSess st
    (scanNextPage st >>= finishScanSess)
    (beginScanSess st >>= scanNextPage >>= finishScanSess)

handleSPC :: St -> EventM () (Next St)
handleSPC st = suspendAndResume $ ifScanSess st
    (scanNextPage st)
    (beginScanSess st >>= scanNextPage)

handleESC :: St -> EventM () (Next St)
handleESC st = ifScanSess st
    ((liftIO . abortScanSess) st >>= continue)
    (continue st)

beginScanSess :: St -> IO St
beginScanSess st = do
    temp <- getTemporaryDirectory
        >>= \tmpdir -> createTempDirectory tmpdir "sscan"
    return $ st
        & stScanningSession .~ (Just temp)
        & stPageCount .~ (Just 0)

abortScanSess :: St -> IO St
abortScanSess st = do
    maybe (return ())
        removeDirectoryRecursive
        (st^.stScanningSession)
    return $ st
        & stScanningSession .~ Nothing
        & stPageCount .~ Nothing

finishScanSess :: St -> IO St
finishScanSess st = do
    void $ forkFinally (finishScanSess' st) $ \result ->
        case result of
          Right _ -> maybe (return ())
              removeDirectoryRecursive
              (st^.stScanningSession)
    return $ st
        & stScanningSession .~ Nothing
        & stPageCount .~ Nothing

-- run OCRmyPDF, pdftk etc., and if any process existed non-zero,
-- record to a log file in the outdir
finishScanSess' :: St -> IO St
finishScanSess' st = undefined

scanNextPage :: St -> IO St
scanNextPage st = undefined

handleHotKey :: St -> Char -> EventM () (Next St)
handleHotKey st 'q' = handleQ st
handleHotKey st ' ' = handleSPC st
handleHotKey st 'o' = updateStateOutsideSession st
    (\s -> s & stOCR .~ (not $ st^.stOCR))
handleHotKey st 'c' = updateStateOutsideSession st
    (\s -> s & stColour .~ (cycleColour $ st^.stColour))
handleHotKey st 'p' = updateStateOutsideSession st
    (\s -> s & stPaper .~ (cyclePaper $ st^.stPaper))
handleHotKey st c = updateStateOutsideSession st $
    case lookupPreset c of
      Just (Preset _ _ f) -> f
      _                   -> id

updateStateOutsideSession :: St -> (St -> St) -> EventM () (Next St)
updateStateOutsideSession st f = continue $ ifScanSess st st (f st)

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

main = do
    home <- getHomeDirectory
    papersize <- init <$> readFile "/etc/papersize"
    let paper = if papersize == "letter" then Letter else A4
        initialState = St
            { _stScanningSession = Nothing
            , _stPageCount       = Nothing
            , _stOCR             = True
            , _stColour          = Greyscale
            , _stPaper           = paper
            , _stDefaultPaper    = paper
            , _stDPI             = 300
            , _stOutFormat       = PDF
            , _stOutdir          = home </> "tmp"
            }
    void $ defaultMain theApp initialState
