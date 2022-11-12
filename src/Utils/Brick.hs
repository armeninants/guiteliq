{-
Module      : Utils.Brick
Description : Brick Utilities
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

Brick Utilities.
-}

module Utils.Brick where

import Data.Char (toUpper)
import qualified Data.Text as T
import Graphics.Vty.Input.Events hiding (Event)
import RIO

modifsToText :: [Modifier] -> Text
modifsToText = mconcat . fmap ((<> "+") . go)
  where
    go MShift = "Shift"
    go MCtrl = "Ctrl"
    go MMeta = "Meta"
    go MAlt = "Alt"

keyToText :: Key -> Text
keyToText = \case
  KEsc -> "Esc"
  KChar c -> toUpper c & T.singleton
  KBS -> "BS"
  KEnter -> "Enter"
  KLeft -> "Left"
  KRight -> "Right"
  KUp -> "Up"
  KDown -> "Down"
  KUpLeft -> "UpLeft"
  KUpRight -> "UpRight"
  KDownLeft -> "DownLeft"
  KDownRight -> "DownRight"
  KCenter -> "Center"
  KFun n -> "Fun" <> T.pack (show n)
  KBackTab -> "BackTab"
  KPrtScr -> "PrtScr"
  KPause -> "Pause"
  KIns -> "Ins"
  KHome -> "Home"
  KPageUp -> "PageUp"
  KDel -> "Del"
  KEnd -> "End"
  KPageDown -> "PageDown"
  KBegin -> "Begin"
  KMenu -> "Menu"
