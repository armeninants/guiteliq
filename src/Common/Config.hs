{-
Module      : Config
Description : Common Config Module
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental
-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Config
  ( module Common.Config,
  )
where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as B
import Lens.Micro.TH (makeLenses)
import RIO
import RIO.FilePath ((</>))
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    getHomeDirectory,
  )
import System.FilePath (takeDirectory)
import Utils.Shell

data ConfigData = ConfigData
  { _readingsDirD :: FilePath,
    _writingsDirD :: FilePath,
    _editorPrefix :: String,
    _latexCmdD :: String,
    _mdCmdD :: String,
    _pdfCmdD :: String,
    _djvuCmdD :: String
  }

instance ToJSON ConfigData where
  toJSON ConfigData {..} =
    object
      [ "readings.root.directory" .= _readingsDirD,
        "writings.root.directory" .= _writingsDirD,
        "editor.prefix" .= _editorPrefix,
        "latex.cmd.prefix" .= _latexCmdD,
        "md.cmd.prefix" .= _mdCmdD,
        "pdf.cmd.prefix" .= _pdfCmdD,
        "djvu.cmd.prefix" .= _djvuCmdD
      ]

instance FromJSON ConfigData where
  parseJSON = withObject "ConfigData" $ \v ->
    ConfigData
      <$> v
      .: "readings.root.directory"
      <*> v
      .: "writings.root.directory"
      <*> v
      .: "editor.prefix"
      <*> v
      .: "latex.cmd.prefix"
      <*> v
      .: "md.cmd.prefix"
      <*> v
      .: "pdf.cmd.prefix"
      <*> v
      .: "djvu.cmd.prefix"

data Config = Config
  { _homeDir :: FilePath,
    _readingsDir :: FilePath,
    _writingsDir :: FilePath,
    _templatesDir :: FilePath,
    _configPath :: FilePath,
    _editorCmd :: String,
    _latexCmd :: String,
    _mdCmd :: String,
    _pdfCmd :: String,
    _djvuCmd :: String
  }

makeLenses ''ConfigData
makeLenses ''Config

defaultConfigData :: ConfigData
defaultConfigData =
  ConfigData
    { _readingsDirD = "Documents/Library",
      _writingsDirD = "Documents/Writings",
      _editorPrefix = "code",
      _latexCmdD = "code",
      _mdCmdD = "code",
      _pdfCmdD = "open",
      _djvuCmdD = "open"
    }

-- | Creates config if does not exist.
getConfig :: MonadIO m => m Config
getConfig = liftIO $ do
  home <- getHomeDirectory
  let confDir = home </> ".guiteliq"
      confPath = confDir </> "config.json"
      tmplsDir = confDir </> "templates"
  confExists <- doesFileExist confPath
  confData <-
    if confExists
      then do
        input <- B.readFile confPath
        let mConfData = decode input :: Maybe ConfigData
        maybe (fail $ "Error processing " <> confPath) return mConfData
      else do
        createDirectoryIfMissing True confDir
        toJSON defaultConfigData & encodePretty & B.writeFile confPath
        return defaultConfigData
  let readsDir = home </> (confData ^. readingsDirD)
      writgsDir = home </> (confData ^. writingsDirD)
  return
    Config
      { _homeDir = home,
        _readingsDir = readsDir,
        _writingsDir = writgsDir,
        _templatesDir = tmplsDir,
        _configPath = confPath,
        _editorCmd = confData ^. editorPrefix,
        _latexCmd = confData ^. latexCmdD,
        _mdCmd = confData ^. mdCmdD,
        _pdfCmd = confData ^. pdfCmdD,
        _djvuCmd = confData ^. djvuCmdD
      }

-- -----------------------------------------------
-- Utils
-- -----------------------------------------------

openPathInDefaultEditor :: Config -> FilePath -> IO ()
openPathInDefaultEditor Config {..} path = runCommand_ _editorCmd [path]

-- | Deprecated
openPathInMarkdownEditor :: Config -> FilePath -> IO ()
openPathInMarkdownEditor Config {..} path = runCommand_ _mdCmd [path]

-- | Deprecated
openPathInLaTeXEditor :: Config -> FilePath -> IO ()
openPathInLaTeXEditor Config {..} path = runCommand_ _latexCmd [path]

-- | Open a Markdown document externally.
openMarkdownDocument :: Config -> FilePath -> IO ()
openMarkdownDocument Config {..} path = runCommand_ _mdCmd [takeDirectory path, path]

-- | Open a LaTeX document externally.
openLaTeXDocument :: Config -> FilePath -> IO ()
openLaTeXDocument Config {..} path = runCommand_ _latexCmd [takeDirectory path, path]