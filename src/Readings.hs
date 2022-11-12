{-
Usage:
    readings open-project --readings-path <path_to_readings>
        Lists all latex files in the `readings-path` directory, and allows to open.

Useful links:
    * https://www.stackage.org/haddock/lts-19.21/brick-0.68.1/Brick-Widgets-List.html
        Documentation
    * https://github.com/2mol/pboy/blob/main/src/UI.hs
        Example project
    * https://github.com/jtdaugherty/brick/blob/master/programs/ListDemo.hs
        Brick List Widget demo
-}

module Readings where

import LoadEnv (loadEnvFrom)
import RIO hiding (on)
import RIO.Process
  ( HasProcessContext (..),
    ProcessContext,
    mkDefaultProcessContext,
  )
import Readings.Core
import Readings.Logger (loggerOptions)
import Readings.Options
  ( Options (Options, optionsEnvFile),
    parseOptions,
  )
import Readings.Settings
  ( AppSettings (..),
    HasSettings (..),
    loadSettings,
  )

-- ---------------------------------------------
-- Main
-- ---------------------------------------------

main :: IO ()
main = do
  Options {..} <- parseOptions
  traverse_ loadEnvFrom optionsEnvFile
  settings <- loadSettings
  lo <- loggerOptions settings
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          ReadingsApp
            { appLogFunc = lf,
              appProcessContext = pc,
              appSettings = settings
            }
     in runRIO app runApp

-- ---------------------------------------------
-- Application
-- ---------------------------------------------

data ReadingsApp = ReadingsApp
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appSettings :: !AppSettings
  }

instance HasLogFunc ReadingsApp where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext ReadingsApp where
  processContextL =
    lens appProcessContext (\x y -> x {appProcessContext = y})

instance HasSettings ReadingsApp where
  settingsL = lens appSettings $ \x y -> x {appSettings = y}

type AppMonad = RIO ReadingsApp

runApp :: AppMonad ()
runApp = do
  settings <- view settingsL
  logDebug $ displayShow settings
  void $ liftIO runBrickApp
