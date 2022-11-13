{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}

{-
Module      : Interface.List
Description : Semantic Interface for List Views
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

Semantic Interface for List Views
-}

module Interface.ListInterface
  ( module Interface.ListInterface,
    module X,
  )
where

import qualified Data.Vector as Vec
import Graphics.Vty.Input.Events hiding (Event)
import Interface.Attribute
import qualified Interface.Attribute as X
import RIO hiding (Vector)

-- ---------------------------------------------

type KeyBinding = (Key, [Modifier])

-- ---------------------------------------------

class
  HasListInterface conf itemModel newItemAttrs
    | conf -> itemModel newItemAttrs
  where
  attrsDescVector :: AttrList conf newItemAttrs

  createNewItem :: Func newItemAttrs (RIO conf (Maybe itemModel))

  -- | An initiation function
  getItems :: RIO conf (Vec.Vector itemModel)

  getConfig :: IO conf

  -- | Action that should be run at program initiation
  initAction :: RIO conf ()

  -- | A set of global actions
  globalActions :: [(Text, KeyBinding, RIO conf ())]

  -- | A set of actions for items: open, edit
  itemActions :: [(Text, KeyBinding, itemModel -> RIO conf ())]

  newItemCallback :: itemModel -> RIO conf ()

  -- | A rendering function for items
  renderL :: itemModel -> Text

  renderR :: itemModel -> Text

  -- | A filtering function for items:
  matchItem :: Text -> itemModel -> Bool

  itemName :: Text

  appName :: Text
