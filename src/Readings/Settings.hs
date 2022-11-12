{-
Module      : Readings.Settings
Description : Settings of Readings App
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

Settings of Readings App
-}

module Readings.Settings
  ( AppSettings (..),
    HasSettings (..),
    loadSettings,
  )
where

import Env
import GHC.Unicode (toLower)
import RIO hiding (Reader)

data AppSettings = AppSettings
  { appVerbose :: !Bool,
    appLogLevel :: !LogLevel,
    appLogFile :: !String
  }
  deriving (Show)

class HasSettings env where
  settingsL :: Lens' env AppSettings

instance HasSettings AppSettings where
  settingsL = id

loadSettings :: IO AppSettings
loadSettings =
  parse id $
    AppSettings
      <$> var auto "LOG_VERBOSE" (def False)
      <*> var logLevel "LOG_LEVEL" (def LevelInfo)
      <*> var str "LOG_FILE" (def "")

logLevel :: Reader e LogLevel
logLevel x = case map toLower x of
  "debug" -> Right LevelDebug
  "info" -> Right LevelInfo
  "warn" -> Right LevelWarn
  "error" -> Right LevelError
  _ -> Right LevelInfo
