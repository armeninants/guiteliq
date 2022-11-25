{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-
Module      : Readings.Core
Description : Core of the Readings App
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

Core of Readings App
-}
module Readings.Core where

import Common.Config
import qualified Common.Config as Conf
import Conduit
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Graphics.Vty.Input.Events
import Interface.DOM
import RIO hiding (on)
import RIO.List (sortBy)
import Readings.Model
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

createNewItem :: RIO Conf.Config (Maybe DocMetadata)
createNewItem = createNewReading

getItems :: RIO Conf.Config (Vec.Vector DocMetadata)
getItems = view Conf.readingsDir >>= liftIO . getDocsMetaSorted

renderL :: DocMetadata -> Text
renderL DocMetadata {..} = _docTitle

-- renderR DocMetadata {..} = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" _docModificationTime
renderR :: DocMetadata -> Text
renderR _ = ""

globalActions :: [(Text, (Key, [Modifier]), RIO Conf.Config ())]
globalActions = [("Open config", (KChar 'g', [MCtrl]), openConfig)]

itemActions :: [(Text, (Key, [Modifier]), DocMetadata -> RIO Conf.Config ())]
itemActions = [("Open", (KEnter, []), openReading)]

newItemCallback :: DocMetadata -> RIO Conf.Config ()
newItemCallback = openReading

matchItem :: Text -> DocMetadata -> Bool
matchItem query_ DocMetadata {..} =
  let query = T.toLower $ T.strip query_
   in (T.null query || T.isInfixOf query (T.toLower _docTitle))

initAction :: RIO Conf.Config ()
initAction = return ()

itemName :: Text
itemName = "reading"

appName :: Text
appName = "readings"

-- runBrickApp :: IO (BrickState Conf.Config DocMetadata
-- runBrickApp = runBrickAppG @Conf.Config @DocMetadata
