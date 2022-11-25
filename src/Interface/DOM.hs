{-
Module      : Interface.DOM
Description : Document Organization Model
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

Document Organization Model
-}
module Interface.DOM where

import Conduit
import Data.Kind (Type)
import RIO
import RIO.FilePath ((</>))
import System.Directory (listDirectory)

-- | This datatype describes the categories of paths within the application.
-- The categories MUST be jointly exclusive and pairwise disjoint.
-- This datatype is general-purpose due to the data parameters
-- `dirCategory` and `fileCategory`.
data PathCategory dirMeta docMeta
  = FPath !(Maybe docMeta)
  | DPath !dirMeta
  | OtherPath

class DocumentOrganizationModel dom where
  type DocMeta dom :: Type
  type DirMeta dom :: Type

  classifyPath :: Proxy dom -> FilePath -> IO (PathCategory (DirMeta dom) (DocMeta dom))

  traversalPolicy :: Proxy dom -> DirMeta dom -> Bool

-- | Source a directory (recursively) based on a document organization model.
sourceDOM ::
  (DocumentOrganizationModel dom, MonadResource m) =>
  Proxy dom ->
  FilePath ->
  ConduitT i (DocMeta dom) m ()
sourceDOM proxy dir = do
  fps <- listDirectory dir & liftIO <&> fmap (dir </>)
  forM fps (classifyPath proxy) <&> zip fps & liftIO >>= go
  where
    go [] = return ()
    go ((fp, pathCat) : l') = do
      case pathCat of
        FPath (Just docMeta) -> do
          yield docMeta
        DPath dirCategory | traversalPolicy proxy dirCategory -> sourceDOM proxy fp
        _other -> return ()
      go l'
