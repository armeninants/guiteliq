{-
Module      : Writings.Core
Description : Core of Writings App
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

Core of Writings App
-}
{-# LANGUAGE TemplateHaskell #-}

module Writings.Model where

import Conduit
import Control.Lens hiding (cons, view, (^.))
import Data.List (isSuffixOf)
import qualified Data.Streaming.Filesystem as F
import Data.Time (UTCTime)
import GHC.Utils.Misc (getModificationUTCTime)
import Interface.DOM
import RIO hiding (on)
import System.FilePath.Posix (takeExtension)
import Utils.LaTeX
import Utils.Markdown

-- ------------------------------------------
-- Document Organization
-- ------------------------------------------

-- | The app's document organization model.
data AppDOM

-- | Supported document type.
data DocType = LaTeX | Markdown
  deriving (Show, Eq, Ord)

-- | Document metadata.
-- Data about documents needed for representing in a list and accessing.
data DocMetadata = DocMetadata
  { _docTitle :: Text,
    _docModificationTime :: UTCTime,
    _docPath :: FilePath,
    _docType :: DocType,
    _docSymbolic :: Bool
  }
  deriving (Show, Eq, Ord)

data DirMetadata = DirMetadata
  { _dirPath :: FilePath,
    _dirSymbolic :: Bool
  }
  deriving (Show, Eq, Ord)

makeLenses ''DocMetadata
makeLenses ''DirMetadata

instance DocumentOrganizationModel AppDOM where
  type
    DirMeta AppDOM =
      DirMetadata

  type
    DocMeta AppDOM =
      DocMetadata

  classifyPath Proxy fp = do
    ft <- F.getFileType fp
    case ft of
      _ | ft `elem` [F.FTFile, F.FTFileSym] ->
        case takeExtension fp of
          ".tex"
            | not (".inc.tex" `isSuffixOf` fp) ->
                helper getTeXTitle fp ft LaTeX
          ".md"
            | not (".inc.md" `isSuffixOf` fp) ->
                helper getMarkdownTitle fp ft Markdown
          _ -> return $ FPath Nothing
      _
        | ft `elem` [F.FTDirectory, F.FTDirectorySym] ->
            return $ DPath $ DirMetadata fp (ft /= F.FTDirectory)
      _ -> return OtherPath
  traversalPolicy Proxy _dirCat = True

helper ::
  (FilePath -> IO (Maybe Text)) ->
  FilePath ->
  F.FileType ->
  DocType ->
  IO (PathCategory DirMetadata DocMetadata)
helper titleGetter fp ft doc = do
  mtitle <- titleGetter fp
  case mtitle of
    Nothing -> return $ FPath Nothing
    Just title -> do
      modificationTime <- liftIO $ getModificationUTCTime fp
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
