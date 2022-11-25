{-
Usage:
    writings open-project --writings-path <path_to_writings>
        Lists all latex files in the `writings-path` directory, and allows to open.

Useful links:
    * https://www.stackage.org/haddock/lts-19.21/brick-0.68.1/Brick-Widgets-List.html
        Documentation
    * https://github.com/2mol/pboy/blob/main/src/UI.hs
        Example project
    * https://github.com/jtdaugherty/brick/blob/master/programs/ListDemo.hs
        Brick List Widget demo
-}

module Writings where

import LoadEnv (loadEnvFrom)
import RIO hiding (on)
import RIO.Process
  ( HasProcessContext (..),
    ProcessContext,
    mkDefaultProcessContext,
  )
import Writings.Logger (loggerOptions)
import Writings.Options
  ( Options (Options, optionsEnvFile),
    parseOptions,
  )
import Writings.Settings
  ( AppSettings (..),
    HasSettings (..),
    loadSettings,
  )
import Writings.TUI

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
          WritingsApp
            { appLogFunc = lf,
              appProcessContext = pc,
              appSettings = settings
            }
     in runRIO app runApp

-- ---------------------------------------------
-- Application
-- ---------------------------------------------

data WritingsApp = WritingsApp
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appSettings :: !AppSettings
  }

instance HasLogFunc WritingsApp where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext WritingsApp where
  processContextL =
    lens appProcessContext (\x y -> x {appProcessContext = y})

instance HasSettings WritingsApp where
  settingsL = lens appSettings $ \x y -> x {appSettings = y}

type AppMonad = RIO WritingsApp

runApp :: AppMonad ()
runApp = do
  settings <- view settingsL
  logDebug $ displayShow settings
  void $ liftIO runBrickApp
