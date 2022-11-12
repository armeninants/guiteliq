{-# LANGUAGE TemplateHaskell #-}

{-
Module      : Writings.Types
Description : Types of Writings App
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

Config of Writings App
-}

module Writings.Types where

import Control.Lens hiding (cons, (^.))
import Data.Time (UTCTime)
import RIO hiding (on)

data DocType = LaTeX

-- | Document metadata.
-- Data about documents needed for representing in a list and accessing.
data DocMetadata = DocMetadata
  { _docTitle :: Text,
    _docModificationTime :: UTCTime,
    _docPath :: FilePath,
    _docType :: DocType,
    _docSymbolic :: Bool
  }

data DirMetadata = DirMetadata
  { _dirPath :: FilePath,
    _dirSymbolic :: Bool
  }

makeLenses ''DocMetadata
