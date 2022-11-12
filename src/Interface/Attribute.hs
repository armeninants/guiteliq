{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

{-
Module      : Interface.Attribute
Description : Interface.Attribute
Copyright   : (c) Armen Inants, 2022
License     : MIT
Maintainer  : armen@inants.com
Stability   : experimental

An attribute is a generic specification of a function argument.
Such a specification includes:
- the type of the attribute
- the name of the attribute
- how an attribute's value is obtained: e.g. direct user input, multiple choice, etc.
- the initial (default) value

To describe the attribute signature of a function means to provide
a list of attributes with all the information mentioned above.

-}

module Interface.Attribute
  ( Attribute (..),
    TextAttr (..),
    MultChoiceAttr (..),
    AttributeInit (..),
    ToFunc (..),
    HeterogenList (..),
    BrickAttrList (..),
    All,
  )
where

import Data.Kind
import RIO

-- ---------------------------------------------
-- Helper type families
-- ---------------------------------------------

type family
  All
    (c :: Type -> Constraint)
    (ts :: [Type]) ::
    Constraint
  where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

-- ---------------------------------------------
-- Attribute Description
-- ---------------------------------------------

class Attribute attr where
  type AttrValType attr :: Type
  type AttrInitType attr :: Type
  attrLabel :: attr -> Text

  -- attrInitial :: attr -> InSomeMonad (AttrInitType attr)
  attrValid :: attr -> AttrValType attr -> Bool

class Attribute attr => AttributeInit conf attr where
  attrInitial :: attr -> RIO conf (AttrInitType attr)

-- ---------------------------------------------

data TextAttr conf = TextAttr
  { tLabel :: Text,
    tInitial :: RIO conf Text,
    tValid :: Text -> Bool
  }

data MultChoiceAttr conf = MultChoiceAttr
  { mLabel :: Text,
    mInitial :: RIO conf [Text],
    mValid :: Maybe (Int, Text) -> Bool
  }

instance Attribute (TextAttr conf) where
  type AttrValType (TextAttr conf) = Text
  type AttrInitType (TextAttr conf) = Text
  attrLabel = tLabel
  attrValid = tValid

instance Attribute (MultChoiceAttr conf) where
  type AttrValType (MultChoiceAttr conf) = Maybe (Int, Text)
  type AttrInitType (MultChoiceAttr conf) = [Text]
  attrLabel = mLabel
  attrValid = mValid

instance AttributeInit conf (TextAttr conf) where
  attrInitial = tInitial

instance AttributeInit conf (MultChoiceAttr conf) where
  attrInitial = mInitial

-- -----------------------------------------------

class ToFunc (attrList :: [Type]) (res :: Type) where
  type Func attrList res :: Type

instance ToFunc '[] res where
  type Func '[] res = res

instance ToFunc (attr ': attrs) res where
  type Func (attr ': attrs) res = AttrValType attr -> Func attrs res

-- -----------------------------------------------

data BrickAttrList :: Type -> [Type] -> Type where
  NoAttr :: BrickAttrList conf '[]
  (:>) :: AttributeInit conf attr => attr -> BrickAttrList conf attrs -> BrickAttrList conf (attr ': attrs)

infixr 5 :>

-- -----------------------------------------------

class HeterogenList (a :: [Type] -> Type) where
  hLength :: a ts -> Int

  hNull :: a ts -> Bool
  hNull = (== 0) . hLength

  hEmpty :: a '[]

instance HeterogenList (BrickAttrList conf) where
  hLength NoAttr = 0
  hLength (_ :> ts) = 1 + hLength ts
  hEmpty = NoAttr
