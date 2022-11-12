{-
Module      : Utils.Text
Description : Text Utilities
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

Text Utilities.
-}

module Utils.Text where

import Data.Char (toUpper)
import qualified Data.Text as T
import RIO hiding (on)

mapHead :: (Char -> Char) -> Text -> Text
mapHead f x =
  case T.uncons x of
    Just (c, cs) -> T.singleton (f c) <> cs
    Nothing -> x

capitalise :: Text -> Text
capitalise = mapHead toUpper

allCaps :: Text -> Text
allCaps = T.map toUpper
