{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TemplateHaskellQuotes  #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Algebra.Actions where

import           Algebra.Basics
import           Data.Kind (type (*))
import           Data.Proxy
-- import           GHC.TypeLits (Symbol)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Prelude                   hiding (Monoid, negate, recip, (*),
                                            (+), (-), (/))
import qualified Prelude                   as P

-- data ActionTag = Self BinaryTag | Scalar BinaryTag

class LeftActs' (tag :: BinaryTag) a b where
  leftAct' :: Proxy tag -> a -> b -> b

class RightActs' (tag :: BinaryTag) a b where
  rightAct' :: Proxy tag -> b -> a -> b

class (LeftActs' ltag a b, RightActs' rtag a b) =>
      Acts ltag rtag a b

-- I could see this turning into a (type|data) family
data IsLinear     = YesLinear     | NotLinear
data IsCompatible = YesCompatible | NotCompatible

-- | Associative/semigroup actions

-- | The structure of 'b' is tagged by 'tag'.
class (Semigroup ao a, Semigroup bo b) =>
      AssocLeftActs ao a bo b action where
  leftAct :: Proxy ao -> Proxy bo -> Proxy action -> a -> b -> b

  -- | (a ao a') `leftAct` b = (a `leftAct` b) bo (a' `leftAct` b)
  type IsLeftActorLinear ao a bo b action :: IsLinear

  -- | a `leftAct` (b bo b') = (a `leftAct` b) bo (a `leftAct` b')
  type IsLeftActedLinear ao a bo b action :: IsLinear

  -- | (a ao a') `leftAct` b = a `leftAct` (a' `leftAct` b)
  type IsLeftCompatible ao a bo b action :: IsCompatible

  -- type instance IsLeftActorLinear ao a bo b action = NotLinear
  -- type instance IsLeftActedLinear ao a bo b action = NotLinear
  -- type instance IsLeftCompatible  ao a bo b action = NotLinear

type LeftActorLinear ao a bo b action = IsLeftActorLinear ao a bo b action ~ YesLinear
type LeftActedLinear ao a bo b action = IsLeftActedLinear ao a bo b action ~ YesLinear
type LeftCompatible  ao a bo b action = IsLeftCompatible  ao a bo b action ~ YesCompatible

type LeftLinear ao a bo b action =
  (LeftActorLinear ao a bo b action, LeftActedLinear ao a bo b action)

-- | The structure of 'b' is tagged by 'tag'.
class (Semigroup ao a, Semigroup bo b) =>
      AssocRightActs ao a bo b action where
  rightAct :: Proxy ao -> Proxy bo -> Proxy action -> b -> a -> b

  -- | These are with a flipped 'rightAct'
  -- | (a ao a') `rightAct` b = (a `rightAct` b) bo (a' `rightAct` b)
  type IsRightActorLinear ao a bo b action :: IsLinear

  -- | a `rightAct` (b bo b') = (a `rightAct` b) bo (a `rightAct` b')
  type IsRightActedLinear ao a bo b action :: IsLinear

  -- | (a ao a') `rightAct` b = a `rightAct` (a' `rightAct` b)
  type IsRightCompatible ao a bo b action :: IsCompatible

type RightActorLinear ao a bo b action = IsRightActorLinear ao a bo b action ~ YesLinear
type RightActedLinear ao a bo b action = IsRightActedLinear ao a bo b action ~ YesLinear
type RightCompatible  ao a bo b action = IsRightCompatible  ao a bo b action ~ YesCompatible

type RightLinear ao a bo b action =
  (RightActorLinear ao a bo b action, RightActedLinear ao a bo b action)

-- | FIXME: tons of distributivity implied hereafter in the actions, no?

-- class (AssocLeftActs op a ltag b, AssocRightActs op a rtag b) =>
--       AssocActs op a ltag rtag b

-- class (Group op a, AssocLeftActs op a ltag b) => GSet op a ltag b

-- instance (Group op a) => AssocLeftActs op a op a where
--   leftAct _ _ = binaryOp (Proxy :: Proxy op)

instance (Ring Add mul a) => AssocLeftActs Add a Add a mul where
  leftAct _ _ mulProxy = binaryOp mulProxy

  type IsLeftActorLinear Add a Add a mul = YesLinear
  type IsLeftActedLinear Add a Add a mul = YesLinear
  type IsLeftCompatible  Add a Add a mul = NotCompatible

instance (Ring add Mul a) => AssocLeftActs Mul a add a Mul where
  leftAct _ _ mulProxy = binaryOp mulProxy

  type IsLeftActorLinear Mul a add a Mul = NotLinear
  type IsLeftActedLinear Mul a add a Mul = NotLinear
  type IsLeftCompatible  Mul a add a Mul = YesCompatible

instance (Ring Add mul a) => AssocRightActs Add a Add a mul where
  rightAct _ _ mulProxy = binaryOp mulProxy

  type IsRightActorLinear Add a Add a mul = YesLinear
  type IsRightActedLinear Add a Add a mul = YesLinear
  type IsRightCompatible  Add a Add a mul = NotCompatible

instance (Ring add Mul a) => AssocRightActs Mul a add a Mul where
  rightAct _ _ mulProxy = binaryOp mulProxy

  type IsRightActorLinear Mul a add a Mul = NotLinear
  type IsRightActedLinear Mul a add a Mul = NotLinear
  type IsRightCompatible  Mul a add a Mul = YesCompatible
