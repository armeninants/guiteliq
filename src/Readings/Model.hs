{-
Module      : Readings.Core
Description : Core of Readings App
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

Core of Readings App
-}
{-# LANGUAGE TemplateHaskell #-}

module Readings.Model
  ( AppDOM,
    DocMetadata (..),
    DocType (..),
    docTitle,
    docModificationTime,
    docPath,
    docType,
    docSymbolic,
  )
where

import Conduit
import Control.Lens hiding (cons, view, (^.))
import qualified Data.Streaming.Filesystem as F
import qualified Data.Text as T
import Data.Time (UTCTime)
import Interface.DOM
import RIO hiding (on)
import System.Directory (getModificationTime)
import System.FilePath.Posix (takeBaseName, takeExtension)

-- ------------------------------------------
-- Document Organization
-- ------------------------------------------

-- | The app's document organization model.
data AppDOM

-- | Supported document type.
data DocType = PDF | DJVU
  deriving (Show, Eq, Ord)

-- | Document metadata.
-- Data about documents needed for representing in a list and accessing.
data DocMetadata = DocMetadata
  { _docTitle :: !Text,
    _docModificationTime :: !UTCTime,
    _docPath :: !FilePath,
    _docType :: !DocType,
    _docSymbolic :: !Bool
  }
  deriving (Show, Eq, Ord)

data DirMetadata = DirMetadata
  { _dirPath :: !FilePath,
    _dirSymbolic :: !Bool
  }
  deriving (Show, Eq, Ord)

makeLenses ''DocMetadata

instance DocumentOrganizationModel AppDOM where
  type DirMeta AppDOM = DirMetadata
  type DocMeta AppDOM = DocMetadata

  classifyPath Proxy fp = do
    ft <- F.getFileType fp
    case ft of
      _ | ft `elem` [F.FTFile, F.FTFileSym] ->
        case takeExtension fp of
          ".pdf" -> helper fp ft PDF
          ".djvu" -> helper fp ft DJVU
          _ -> return $ FPath Nothing
      _
        | ft `elem` [F.FTDirectory, F.FTDirectorySym] ->
          return $ DPath $ DirMetadata fp (ft /= F.FTDirectory)
      _ -> return OtherPath
  traversalPolicy Proxy _dirCat = True

helper :: FilePath -> F.FileType -> DocType -> IO (PathCategory DirMetadata DocMetadata)
helper fp ft doc = do
  let title = takeBaseName fp & T.pack
  modificationTime <- liftIO $ getModificationTime fp
  return $
    FPath $
      Just $
        DocMetadata
          { _docTitle = title,
            _docModificationTime = modificationTime,
            _docPath = fp,
            _docType = doc,
            _docSymbolic = ft /= F.FTFile
          }
