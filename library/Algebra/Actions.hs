{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
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
-- import           Language.Haskell.TH
-- import           Language.Haskell.TH.Quote
import           Prelude        hiding (Monoid, negate, recip, (*), (+), (-),
                                 (/))
import qualified Prelude        as P

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

class
   ( Semigroup ao a
   , Semigroup bo b)
  => AssocLeftActs
        (ao :: BinaryTag) (a :: *) (bo :: BinaryTag) (b :: *) (action :: BinaryTag) where

  leftAct :: Proxy ao -> Proxy bo -> Proxy action -> a -> b -> b
  default leftAct
    :: (a ~ b, Semigroup action a)
    => Proxy ao -> Proxy bo -> Proxy action -> a -> b -> b
  leftAct _ _ actionProxy = binaryOp actionProxy

  -- | (a ao a') `leftAct` b = (a `leftAct` b) bo (a' `leftAct` b)
  type IsLeftActorLinear ao a bo b action :: IsLinear
  type IsLeftActorLinear ao a bo b action = YesLinear

  -- | a `leftAct` (b bo b') = (a `leftAct` b) bo (a `leftAct` b')
  type IsLeftActedLinear ao a bo b action :: IsLinear
  type IsLeftActedLinear ao a bo b action = YesLinear

  -- | (a ao a') `leftAct` b = a `leftAct` (a' `leftAct` b)
  type IsLeftCompatible ao a bo b action :: IsCompatible
  type IsLeftCompatible ao a bo b action = YesCompatible

  -- type instance IsLeftActorLinear ao a bo b action = NotLinear
  -- type instance IsLeftActedLinear ao a bo b action = NotLinear
  -- type instance IsLeftCompatible  ao a bo b action = NotLinear

type LeftActorLinear ao a bo b action = IsLeftActorLinear ao a bo b action ~ YesLinear
type LeftActedLinear ao a bo b action = IsLeftActedLinear ao a bo b action ~ YesLinear

type LeftLinear ao a bo b action =
  ( AssocLeftActs ao a bo b action
  , LeftActorLinear ao a bo b action
  , LeftActedLinear ao a bo b action
  )

type LeftCompatible  ao a bo b action =
  ( AssocLeftActs     ao a bo b action
  , IsLeftCompatible  ao a bo b action ~ YesCompatible
  )

-- | The structure of 'b' is tagged by 'tag'.
class
   ( Semigroup ao a
   , Semigroup bo b)
  => AssocRightActs
        (ao :: BinaryTag) (a :: *) (bo :: BinaryTag) (b :: *) (action :: BinaryTag) where
  rightAct :: Proxy ao -> Proxy bo -> Proxy action -> b -> a -> b
  default rightAct
    :: (a ~ b, Semigroup action a)
    => Proxy ao -> Proxy bo -> Proxy action -> b -> a -> b
  rightAct _ _ actionProxy = binaryOp actionProxy

  -- | These are with a flipped 'rightAct'
  -- | (a ao a') `rightAct` b = (a `rightAct` b) bo (a' `rightAct` b)
  type IsRightActorLinear ao a bo b action :: IsLinear
  type IsRightActorLinear ao a bo b action = NotLinear

  -- | a `rightAct` (b bo b') = (a `rightAct` b) bo (a `rightAct` b')
  type IsRightActedLinear ao a bo b action :: IsLinear
  type IsRightActedLinear ao a bo b action = NotLinear -- or equal to prev?

  -- | (a ao a') `rightAct` b = a `rightAct` (a' `rightAct` b)
  type IsRightCompatible ao a bo b action :: IsCompatible
  type IsRightCompatible ao a bo b action = NotCompatible

type RightActorLinear ao a bo b action = IsRightActorLinear ao a bo b action ~ YesLinear
type RightActedLinear ao a bo b action = IsRightActedLinear ao a bo b action ~ YesLinear

type RightLinear ao a bo b action =
  ( AssocRightActs ao a bo b action
  , RightActorLinear ao a bo b action
  , RightActedLinear ao a bo b action
  )

type RightCompatible  ao a bo b action =
  ( AssocRightActs     ao a bo b action
  , IsRightCompatible  ao a bo b action ~ YesCompatible
  )

-- | FIXME: tons of distributivity implied hereafter in the actions, no?

-- class (AssocLeftActs op a ltag b, AssocRightActs op a rtag b) =>
--       AssocActs op a ltag rtag b

-- class (Group op a, AssocLeftActs op a ltag b) => GSet op a ltag b

-- instance (Group op a) => AssocLeftActs op a op a where
--   leftAct _ _ = binaryOp (Proxy :: Proxy op)

instance (Ring Add mul a) => AssocLeftActs Add a Add a mul where
  type IsLeftActorLinear Add a Add a mul = YesLinear
  type IsLeftActedLinear Add a Add a mul = YesLinear

instance (Ring add Mul a) => AssocLeftActs Mul a add a Mul where
  type IsLeftCompatible  Mul a add a Mul = YesCompatible

instance (Ring Add mul a) => AssocRightActs Add a Add a mul where
  type IsRightActorLinear Add a Add a mul = YesLinear
  type IsRightActedLinear Add a Add a mul = YesLinear

instance (Ring add Mul a) => AssocRightActs Mul a add a Mul where
  type IsRightCompatible  Mul a add a Mul = YesCompatible

-- | Abelian groups are Z-(bi)modules.
abelianIntegerAction
  :: (AbelianGroup op a)
  => Proxy add -> Proxy op -> Proxy mul -> Integer -> a -> a
abelianIntegerAction _ opProxy _ 0 _ = neutral opProxy
abelianIntegerAction _ _ _ 1 a = a
abelianIntegerAction addProxy opProxy mulProxy n a =
  binaryOp opProxy a (abelianIntegerAction addProxy opProxy mulProxy (n P.- 1) a)

instance (AbelianGroup op a) =>
         AssocLeftActs Add Integer op a Mul where
  leftAct = abelianIntegerAction
  type IsLeftActorLinear Add Integer op a Mul = YesLinear
  type IsLeftActedLinear Add Integer op a Mul = YesLinear

instance (AbelianGroup op a) =>
         AssocLeftActs Mul Integer op a Mul where
  leftAct = abelianIntegerAction
  type IsLeftCompatible Mul Integer op a Mul = YesCompatible
