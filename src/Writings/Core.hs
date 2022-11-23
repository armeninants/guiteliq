{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-
Module      : Writings.Core
Description : Core Components of Writings App
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental
-}
module Writings.Core where

import App.Config
import qualified App.Config as Conf
import Conduit
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Graphics.Vty.Input.Events
import Interface.DOM
import Interface.ListInterface
import RIO hiding (on)
import RIO.ByteString (writeFile)
import RIO.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import RIO.FilePath (isValid, takeDirectory, takeFileName, (</>))
import RIO.List (sortBy)
import RIO.Time (defaultTimeLocale, formatTime)
import Shelly hiding (path, (</>))
import UI.ListTUI
import Utils.LaTeX
import Writings.Model

mapFst :: (a -> c) -> [(a, b)] -> [(c, b)]
mapFst f xys = [(f x, y) | (x, y) <- xys]

-- ------------------------------------------
-- Actions on Files
-- ------------------------------------------

sortByModif :: [DocMetadata] -> [DocMetadata]
sortByModif = sortBy f'
  where
    f' x y = compare (y ^. docModificationTime) (x ^. docModificationTime)

-- | Get the list
getDocsMeta :: FilePath -> IO (Vector DocMetadata)
getDocsMeta topDir =
  Vec.fromList
    <$> runConduitRes (sourceDOM (Proxy :: Proxy AppDOM) topDir .| sinkList)

getDocsMetaSorted :: FilePath -> IO (Vector DocMetadata)
getDocsMetaSorted =
  fmap (Vec.fromList . sortByModif . Vec.toList) <$> getDocsMeta

getTeXInfo :: FilePath -> IO (Maybe DocMetadata)
getTeXInfo fpath = do
  pathCat <- classifyPath (Proxy :: Proxy AppDOM) fpath
  case pathCat of
    FPath (Just mm) -> return $ Just mm
    _ -> return Nothing

-- | Returns a list of template names –
-- the list of subdirectories
-- of the templates directory.
getTemplateDirs :: RIO Conf.Config [Text]
getTemplateDirs = do
  c <- ask
  subdirs <- liftIO $ listSubdirs (c ^. templatesDir)
  return $ T.pack <$> "markdown" : subdirs

-- | Returns relative names of subdirectories of a given dir
listSubdirs :: FilePath -> IO [FilePath]
listSubdirs dir = fmap takeFileName <$> listSubdirsAbs dir

-- | Including symlinks
listSubdirsAbs :: FilePath -> IO [FilePath]
listSubdirsAbs dir =
  listDirectory dir >>= filterM doesDirectoryExist . fmap (dir </>)

-- | Unrolls the hard-coded templates into the file system.
writeDirIn :: FilePath -> [(FilePath, ByteString)] -> IO ()
writeDirIn dir contents =
  mapM_ (uncurry createAndWriteFile) $ mapFst (dir </>) contents

createAndWriteFile :: FilePath -> ByteString -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path content

-- | Creates a new document, returns the path of the TeX file and whether the project is new
createNewDocument :: Text -> Text -> Maybe (Int, Text) -> RIO Config (Maybe DocMetadata)
createNewDocument title name mTemplate = do
  conf <- ask
  -- printf "Creating a project %s from template %s with a title \"%s\"\n" name template title
  case mTemplate of
    Just (_i, "markdown") -> liftIO $ do
      let mdFilePath = (conf ^. writingsDir) </> T.unpack name </> (T.unpack name <> ".md")
      createAndWriteFile mdFilePath $ "# " <> encodeUtf8 title
      getTeXInfo mdFilePath
    Just (_i, template) -> do
      let tmplDir = (conf ^. templatesDir) </> T.unpack template
          newProjectDir = (conf ^. writingsDir) </> T.unpack name
      projDirExists <- liftIO $ doesDirectoryExist newProjectDir
      let texFilePath = newProjectDir </> (T.unpack name <> ".tex")
      liftIO $
        unless projDirExists $ do
          shelly $
            escaping False $ do
              cp_r tmplDir newProjectDir
              mv (newProjectDir </> "main.tex") texFilePath
          onLaTeXFile texFilePath $ setTitle title
      liftIO $ getTeXInfo texFilePath
    Nothing -> return Nothing

-- -----------------------------------------------

openTemplates :: RIO Config ()
openTemplates = do
  conf <- ask
  view templatesDir >>= liftIO . openPathInDefaultEditor conf

openConfig :: RIO Config ()
openConfig = do
  conf <- ask
  view configPath >>= liftIO . openPathInDefaultEditor conf

openWriting :: DocMetadata -> RIO Config ()
openWriting DocMetadata {..} = do
  conf <- ask
  liftIO $ case _docType of
    LaTeX -> openLaTeXDocument conf _docPath
    Markdown -> openMarkdownDocument conf _docPath

-- | TODO: check whether the new element exists in the vector before adding it
updateListWithDoc ::
  Vector DocMetadata -> DocMetadata -> Vector DocMetadata
updateListWithDoc l newItm = Vec.cons newItm l

-- ---------------------------------------------
-- Writings instance of List Interface
-- ---------------------------------------------

type WIF = [TextAttr Conf.Config, TextAttr Conf.Config, MultChoiceAttr Conf.Config]

instance HasListInterface Conf.Config DocMetadata WIF where
  createNewItem :: Text -> Text -> Maybe (Int, Text) -> RIO Conf.Config (Maybe DocMetadata)
  createNewItem = createNewDocument

  getItems :: RIO Conf.Config (Vec.Vector DocMetadata)
  getItems = view Conf.writingsDir >>= liftIO . getDocsMetaSorted

  renderL :: DocMetadata -> Text
  renderL DocMetadata {..} = _docTitle

  renderR :: DocMetadata -> Text
  renderR DocMetadata {..} = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" _docModificationTime

  globalActions :: [(Text, KeyBinding, RIO Conf.Config ())]
  globalActions = [("Open templates", (KChar 't', [MCtrl]), openTemplates), ("Open config", (KChar 'g', [MCtrl]), openConfig)]

  itemActions :: [(Text, KeyBinding, DocMetadata -> RIO Conf.Config ())]
  itemActions = [("Open", (KEnter, []), openWriting)]

  newItemCallback :: DocMetadata -> RIO Conf.Config ()
  newItemCallback = openWriting

  matchItem :: Text -> DocMetadata -> Bool
  matchItem query_ DocMetadata {..} =
    let query = T.toLower $ T.strip query_
     in (T.null query || T.isInfixOf query (T.toLower _docTitle))

  initAction :: RIO Conf.Config ()
  initAction = return ()

  attrsDescVector :: AttrList Conf.Config WIF
  attrsDescVector =
    TextAttr
      { tLabel = "title",
        tInitial = return "",
        tValid = not . T.null
      }
      :> TextAttr
        { tLabel = "name",
          tInitial = return "",
          tValid = isValid . T.unpack
        }
      :> MultChoiceAttr
        { mLabel = "template",
          mInitial = getTemplateDirs,
          mValid = isJust
        }
      :> NoAttr

  getConfig :: IO Conf.Config
  getConfig = Conf.getConfig

  itemName :: Text
  itemName = "document"

  appName :: Text
  appName = "writings"

runBrickApp :: IO (BrickState Conf.Config DocMetadata WIF)
runBrickApp = runBrickAppG @Conf.Config @DocMetadata @WIF
