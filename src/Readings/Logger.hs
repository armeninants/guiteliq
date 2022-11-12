{-
Module      : Readings
Description : Logging Logic
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

Logging logic
-}

module Readings.Logger
  ( loggerOptions,
  )
where

import RIO
import Readings.Settings

loggerOptions :: AppSettings -> IO LogOptions
loggerOptions AppSettings {..} = do
  hdl <- getHandle
  setLogMinLevel appLogLevel <$> logOptionsHandle hdl appVerbose
  where
    getHandle = return stderr

-- TODO: return a file handle if appLogFile is set
