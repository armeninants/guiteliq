{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}

{-
Module      : Interface.BrickAttribute
Description : Interface.BrickAttribute
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental
-}

module Interface.BrickAttribute
  ( module Interface.BrickAttribute,
  )
where

import qualified Brick
import qualified Brick.Focus as Brick
import qualified Brick.Widgets.Border as Brick
import qualified Brick.Widgets.Edit as Brick
import qualified Brick.Widgets.List as Brick
import Data.Kind
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Interface.Attribute
import RIO

-- -----------------------------------------------

class (Show res, Eq res, Ord res) => BrickResource res where
  itemAttrResource :: Text -> res

-- -----------------------------------------------
-- Class of Brick Attributes
-- -----------------------------------------------

class (AttributeInit conf attr, BrickResource res) => BrickAttribute conf res attr where
  data AttrState conf res attr :: Type

  attrResourceMap :: Proxy conf -> attr -> res
  attrResourceMap _ = itemAttrResource . attrLabel

  getAttrValue :: attr -> AttrState conf res attr -> AttrValType attr

  initAttrState :: attr -> AttrInitType attr -> AttrState conf res attr

  renderAttrWidget ::
    attr ->
    AttrState conf res attr ->
    Brick.FocusRing res ->
    Brick.Widget res

  eventHandler ::
    attr ->
    V.Event ->
    AttrState conf res attr ->
    Brick.EventM res (AttrState conf res attr)

-- -----------------------------------------------
-- Instances of BrickAttribute
-- -----------------------------------------------

instance BrickResource res => BrickAttribute conf res (TextAttr conf) where
  newtype AttrState conf res (TextAttr conf) = TextState {unTextState :: Brick.Editor Text res}
  getAttrValue _ = mconcat . Brick.getEditContents . unTextState
  initAttrState attr = TextState . Brick.editor (attrResourceMap (Proxy @conf) attr) Nothing
  renderAttrWidget attr state focusRing =
    Brick.vLimit 1 $
      Brick.renderEditor
        (Brick.txt . T.unlines)
        (Brick.focusGetCurrent focusRing == Just (attrResourceMap (Proxy @conf) attr))
        (unTextState state)
  eventHandler _ ev = (TextState <$>) . Brick.handleEditorEvent ev . unTextState

instance BrickResource res => BrickAttribute conf res (MultChoiceAttr conf) where
  newtype AttrState conf res (MultChoiceAttr conf) = MultChoiceState {unMultChoiceState :: Brick.List res Text}
  getAttrValue _ = Brick.listSelectedElement . unMultChoiceState
  initAttrState attr init = MultChoiceState $ Brick.list (attrResourceMap (Proxy @conf) attr) (Vec.fromList init) 1
  renderAttrWidget attr state focusRing =
    Brick.vLimit 10 $
      Brick.renderList
        (const Brick.txt)
        (Brick.focusGetCurrent focusRing == Just (attrResourceMap (Proxy @conf) attr))
        (unMultChoiceState state)
  eventHandler _ ev = (MultChoiceState <$>) . Brick.handleListEvent ev . unMultChoiceState

-- -----------------------------------------------
-- Lists of attributes and states
-- -----------------------------------------------

data BrickAttrStateList :: Type -> Type -> [Type] -> Type where
  NoAttrState :: BrickAttrStateList conf res '[]
  (::>) :: BrickAttribute conf res attr => (attr, AttrState conf res attr) -> BrickAttrStateList conf res attrs -> BrickAttrStateList conf res (attr ': attrs)

infixr 5 ::>

-- -----------------------------------------------

instance HeterogenList (BrickAttrStateList conf res) where
  hLength NoAttrState = 0
  hLength (_ ::> ts) = 1 + hLength ts
  hEmpty = NoAttrState

liftInitState ::
  (Applicative m, All (BrickAttribute conf res) attrs) =>
  ( forall attr.
    BrickAttribute conf res attr =>
    attr ->
    m (AttrState conf res attr)
  ) ->
  AttrList conf attrs ->
  m (BrickAttrStateList conf res attrs)
liftInitState _ NoAttr = pure NoAttrState
liftInitState f (attr :> attrs) = liftA2 (::>) ((attr,) <$> f attr) (liftInitState f attrs)

liftMonadic ::
  (Applicative m) =>
  (forall attr. BrickAttribute conf res attr => (attr, AttrState conf res attr) -> m (attr, AttrState conf res attr)) ->
  BrickAttrStateList conf res attrs ->
  m (BrickAttrStateList conf res attrs)
liftMonadic _ NoAttrState = pure NoAttrState
liftMonadic f ((attr ::> attrs)) = liftA2 (::>) (f attr) (liftMonadic f attrs)

mapMethod ::
  (forall attr. BrickAttribute conf res attr => (attr, AttrState conf res attr) -> b) ->
  BrickAttrStateList conf res attrs ->
  [b]
mapMethod _ NoAttrState = []
mapMethod f ((attr ::> attrs)) = f attr : mapMethod f attrs

mapMethod1 ::
  (forall attr. AttributeInit conf attr => (attr -> b)) ->
  AttrList conf attrs ->
  [b]
mapMethod1 _ NoAttr = []
mapMethod1 f ((attr :> attrs)) = f attr : mapMethod1 f attrs

-- -----------------------------------------------

class ApplyFuncToAttrs conf res (attrList :: [Type]) a where
  applyFuncToAttrs :: BrickAttrStateList conf res attrList -> Func attrList a -> a

instance ApplyFuncToAttrs conf res '[] a where
  applyFuncToAttrs NoAttrState f = f

instance (BrickAttribute conf res attr, ApplyFuncToAttrs conf res attrs a) => ApplyFuncToAttrs conf res (attr ': attrs) a where
  applyFuncToAttrs ((attr, state) ::> attrs) f =
    applyFuncToAttrs attrs (f (getAttrValue attr state))

-- -----------------------------------------------
-- Functions on lists of Brick (Attribute, State) pairs
-- -----------------------------------------------

initAttrsStates ::
  All (BrickAttribute conf res) attrs =>
  AttrList conf attrs ->
  RIO conf (BrickAttrStateList conf res attrs)
initAttrsStates = liftInitState f
  where
    f attr = initAttrState attr <$> attrInitial attr

handleWidgetsEvents ::
  V.Event ->
  Text ->
  BrickAttrStateList conf res attrs ->
  Brick.EventM res (BrickAttrStateList conf res attrs)
handleWidgetsEvents ev label = liftMonadic h
  where
    h :: BrickAttribute conf res attr => (attr, AttrState conf res attr) -> Brick.EventM res (attr, AttrState conf res attr)
    h (attr, state) =
      (attr,) <$> if attrLabel attr == label then eventHandler attr ev state else return state

validateAttrsValues :: BrickAttrStateList conf res attrs -> Bool
validateAttrsValues = and . mapMethod (\(attr, attrState) -> attrValid attr (getAttrValue attr attrState))

renderAttrsWidgets ::
  forall attrs res conf.
  BrickAttrStateList conf res attrs ->
  Brick.FocusRing res ->
  Brick.Widget res
renderAttrsWidgets attrStates focusRing =
  intercalate [Brick.vLimit 1 (Brick.fill ' ')] (mapMethod f attrStates) & Brick.vBox
  where
    f :: BrickAttribute conf res attr => (attr, AttrState conf res attr) -> [Brick.Widget res]
    f (attr, state) =
      [ Brick.txt (attrLabel attr),
        Brick.hBorder,
        renderAttrWidget attr state focusRing
      ]

getAttrsNames ::
  AttrList conf attrs ->
  [Text]
getAttrsNames = mapMethod1 attrLabel
