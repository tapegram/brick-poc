{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module UI (main) where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types
  ( BrickEvent (..),
    Widget,
  )
import qualified Brick.Types as T
import Brick.Util (bg, on)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( padAll,
    str,
  )
import qualified Brick.Widgets.Dialog as D
import qualified Graphics.Vty as V

data Choice = Red | Blue | Green
  deriving (Show)

drawUI :: D.Dialog Choice -> [Widget ()]
drawUI d = [ui]
  where
    ui = D.renderDialog d $ C.hCenter $ padAll 1 $ str "This is the dialog body."

appEvent :: D.Dialog Choice -> BrickEvent () e -> T.EventM () (T.Next (D.Dialog Choice))
appEvent st (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt st
    V.EvKey V.KEnter [] -> M.halt st
    _ -> D.handleDialogEvent ev st >>= M.continue
appEvent st _ = M.continue st

initialState :: D.Dialog Choice
initialState = D.dialog (Just "Title") (Just (0, choices)) 50
  where
    choices =
      [ ("Red", Red),
        ("Blue", Blue),
        ("Green", Green)
      ]

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (D.dialogAttr, V.white `on` V.blue),
      (D.buttonAttr, V.black `on` V.white),
      (D.buttonSelectedAttr, bg V.yellow)
    ]

theApp :: M.App (D.Dialog Choice) e ()
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }

main :: IO ()
main = do
  d <- M.defaultMain theApp initialState
  putStrLn $ "You chose: " <> show (D.dialogSelection d)

-- import Brick

-- ui :: Widget ()
-- ui = str "Hello, world!"

-- main :: IO ()
-- main = simpleMain ui