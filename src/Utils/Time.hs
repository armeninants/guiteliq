{-
Module      : Utils.Time
Description : Time Utilities
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental
-}

module Utils.Time where

import RIO.Time (defaultTimeLocale, formatTime)
import Data.Time (UTCTime)

import qualified Data.Text as T
import RIO

formatYMD :: UTCTime -> Text
formatYMD = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"