{-
Module      : Writings.Options
Description : Options of Writings App
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

Options of Writings App
-}

{-# LANGUAGE TemplateHaskell #-}

module Writings.Options
    ( Options(..)
    , parseOptions
    )
where

import           Options.Applicative.Simple
import qualified Paths_guiteliq
import           RIO


newtype Options = Options
    { optionsEnvFile :: Maybe FilePath
    }


optParser :: Parser Options
optParser =
    Options
        <$> optional (strOption
            (  short 'e'
            <> long "env-file"
            <> metavar "ENV"
            <> help "Load environment variables from a file"
            ))


parseOptions :: IO Options
parseOptions = do
    (options, ()) <- simpleOptions
        $(simpleVersion Paths_guiteliq.version)
        "LaTeX Writings Manager"
        "LaTeX Writings Manager"
        optParser
        empty
    return options
