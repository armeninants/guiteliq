{-
Module      : Readings.Options
Description : Options of Readings App
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

Options of Readings App
-}
{-# LANGUAGE TemplateHaskell #-}

module Readings.Options
  ( Options (..),
    parseOptions,
  )
where

import Options.Applicative.Simple
import qualified Paths_guiteliq
import RIO

newtype Options = Options
  { optionsEnvFile :: Maybe FilePath
  }

optParser :: Parser Options
optParser =
  Options
    <$> optional
      ( strOption
          ( short 'e'
              <> long "env-file"
              <> metavar "ENV"
              <> help "Load environment variables from a file"
          )
      )

parseOptions :: IO Options
parseOptions = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_guiteliq.version)
      "LaTeX Readings Manager"
      "LaTeX Readings Manager"
      optParser
      empty
  return options
