{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-
Module      : Readings.Core
Description : Core of Readings App
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

Core of Readings App
-}
module Readings.Core where

import App.Config
  ( Config,
    configPath,
    djvuCmd,
    editorCmd,
    pdfCmd,
  )
import qualified App.Config as Conf
import Conduit
import qualified Data.Text as T
import qualified Data.Vector as Vec
import GHC.Plugins (mapFst)
import Graphics.Vty.Input.Events
import Interface.DOM
import Interface.ListInterface
import RIO hiding (on)
import RIO.ByteString (writeFile)
import RIO.Directory (createDirectoryIfMissing)
import RIO.FilePath (takeDirectory, takeFileName, (</>))
import RIO.List (sortBy)
import Readings.Model
import System.Directory (doesDirectoryExist, listDirectory)
import UI.ListTUI
import Utils.Shell

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
    <$> runConduitRes (sourceDOM (Proxy @AppDOM) topDir .| sinkList)

getDocsMetaSorted :: FilePath -> IO (Vector DocMetadata)
getDocsMetaSorted =
  fmap (Vec.fromList . sortByModif . Vec.toList) <$> getDocsMeta

getTeXInfo :: FilePath -> IO (Maybe DocMetadata)
getTeXInfo fpath = do
  pathCat <- classifyPath (Proxy :: Proxy AppDOM) fpath
  case pathCat of
    FPath (Just mm) -> return $ Just mm
    _ -> return Nothing

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

openPathInDefaultEditor :: Conf.Config -> FilePath -> IO ()
openPathInDefaultEditor conf path = do
  let cmdStr = conf ^. editorCmd
  -- print cmdStr
  runCommand_ cmdStr [path]

-- | The input is a TeX file path
-- openTeXProject :: Conf.Config -> FilePath -> IO ()
-- openTeXProject conf fpath = openPathInDefaultEditor conf $ takeDirectory fpath
createAndWriteFile :: FilePath -> ByteString -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path content

-- | Creates a new document, returns the path of the TeX file and whether the project is new
createNewReading :: RIO Config (Maybe DocMetadata)
createNewReading = do
  return Nothing

-- -----------------------------------------------

openConfig :: RIO Config ()
openConfig = do
  conf <- ask
  view configPath >>= liftIO . openPathInDefaultEditor conf

openReading :: DocMetadata -> RIO Config ()
openReading DocMetadata {..} = do
  conf <- ask
  let cmdStr = conf ^. getCmdLens _docType
  liftIO $ runCommand_ cmdStr [_docPath]
  where
    getCmdLens = \case
      PDF -> pdfCmd
      DJVU -> djvuCmd

-- | TODO: check whether the new element exists in the vector before adding it
updateListWithDoc ::
  Vector DocMetadata -> DocMetadata -> Vector DocMetadata
updateListWithDoc l newItm = Vec.cons newItm l

-- ---------------------------------------------
-- Readings instance of List Interface
-- ---------------------------------------------

type RIF = '[]

instance HasListInterface Conf.Config DocMetadata RIF where
  createNewItem :: RIO Conf.Config (Maybe DocMetadata)
  createNewItem = createNewReading

  getItems :: RIO Conf.Config (Vec.Vector DocMetadata)
  getItems = view Conf.readingsDir >>= liftIO . getDocsMetaSorted

  renderL :: DocMetadata -> Text
  renderL DocMetadata {..} = _docTitle

  -- renderR DocMetadata {..} = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" _docModificationTime
  renderR :: DocMetadata -> Text
  renderR _ = ""

  globalActions :: [(Text, KeyBinding, RIO Conf.Config ())]
  globalActions = [("Open config", (KChar 'g', [MCtrl]), openConfig)]

  itemActions :: [(Text, KeyBinding, DocMetadata -> RIO Conf.Config ())]
  itemActions = [("Open", (KEnter, []), openReading)]

  newItemCallback :: DocMetadata -> RIO Conf.Config ()
  newItemCallback = openReading

  matchItem :: Text -> DocMetadata -> Bool
  matchItem query_ DocMetadata {..} =
    let query = T.toLower $ T.strip query_
     in (T.null query || T.isInfixOf query (T.toLower _docTitle))

  initAction :: RIO Conf.Config ()
  initAction = return ()

  attrsDescVector :: AttrList Conf.Config RIF
  attrsDescVector = hEmpty

  getConfig :: IO Conf.Config
  getConfig = Conf.getConfig

  itemName :: Text
  itemName = "reading"

  appName :: Text
  appName = "readings"

runBrickApp :: IO (BrickState Conf.Config DocMetadata RIF)
runBrickApp = runBrickAppG @Conf.Config @DocMetadata @RIF
