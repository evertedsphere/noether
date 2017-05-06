{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TemplateHaskellQuotes  #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Algebra.Basics where

import           Data.Kind  (type (*))
import           Data.Proxy
-- import           GHC.TypeLits (Symbol)
-- import           Language.Haskell.TH
-- import           Language.Haskell.TH.Quote
import           GHC.Exts
import Data.Type.Equality
import           GHC.TypeLits hiding (type (*))
import Prelude
       hiding (Monoid, negate, recip, (*), (+), (-), (/), fromInteger)
import qualified Prelude      as P
import Unsafe.Coerce (unsafeCoerce)

data UnaryTag = Neg
  deriving Show

data BinaryTag = Add | Mul
  deriving Show

pattern AddP :: Proxy Add
pattern AddP = Proxy

pattern MulP :: Proxy Mul
pattern MulP = Proxy

type family NumericLit (n :: Nat) = (c :: * -> Constraint) where
  NumericLit 0 = Neutral Add
  NumericLit 1 = Neutral Mul
  -- NumericLit 2 = Field Add Mul
  -- NumericLit n = NumericLit (n - 1)
  NumericLit n = Ring Add Mul

zero'
  :: (NumericLit n a)
  => (n ~ 0 => a)
zero' = neutral AddP

one'
  :: (NumericLit n a)
  => (n ~ 1 => a)
one' = neutral MulP

fromIntegerP :: forall n a. (KnownNat n, NumericLit n a) => Proxy n -> a
fromIntegerP p =
  case (sameNat p (Proxy :: Proxy 0)) of
    Just prf -> gcastWith prf zero'
    Nothing -> case sameNat p (Proxy :: Proxy 1) of
      Just prf -> gcastWith prf one'
      Nothing -> undefined -- unsafeCoerce (val (Proxy :: Proxy a))
        -- where
        --   val :: (Field Add Mul b) => Proxy b -> b
        --   val _ = one + undefined -- fromIntegerP (Proxy :: Proxy (n - 1))

fromInteger :: Num a => Integer -> a
fromInteger = P.fromInteger

-- mkLit
--   :: (NumericLit n a)
--   => ((2 <= n) => a)
-- mkLit = one'

-- mkLit'
--   :: (NumericLit (n - 1) a)
--   => (KnownNat n => a)
-- mkLit' = one'

-- fi' :: Integer -> a
-- fi' n = case someNatVal n of
--   Just p -> Just

class UnaryNeutral (op :: UnaryTag) a where
  unaryNeutral :: Proxy op -> a

class UnaryOp (op :: UnaryTag) a where
  unaryOp :: Proxy op -> a -> a

class Magma (op :: BinaryTag) a where
  binaryOp :: Proxy op -> a -> a -> a

class Neutral (op :: BinaryTag) a where
  neutral :: Proxy op -> a

class (Neutral op a) => Invertible op a where
  invert :: Proxy op -> a -> a

class (Magma add a, Magma mul a) => DistributesOver add mul a

-- | Instances for Double

instance Neutral Add Double where neutral _ = fromInteger 0
instance Magma Add Double where binaryOp _ = (P.+)
instance Invertible Add Double where invert _ = P.negate
instance Commutative Add Double
instance Semigroup Add Double
instance DistributesOver Add Mul Double

instance Neutral Mul Double where neutral _ = fromInteger 1
instance Magma Mul Double where binaryOp _ = (P.*)
instance Invertible Mul Double where invert _ = (P./ fromInteger 1)
instance Commutative Mul Double
instance Semigroup Mul Double

-- | Instances for Integer

instance Neutral Add Integer where neutral _ = fromInteger 0
instance Magma Add Integer where binaryOp _ = (P.+)
instance Invertible Add Integer where invert _ = P.negate
instance Commutative Add Integer
instance Semigroup Add Integer
instance DistributesOver Add Mul Integer

instance Neutral Mul Integer where neutral _ = fromInteger 1
instance Magma Mul Integer where binaryOp _ = (P.*)
instance Commutative Mul Integer
instance Semigroup Mul Integer

-- | Classes

-- | Single binary operation

-- FIXME: any dependencies?
class (Magma op a) => Commutative op a

class (Magma op a) => Semigroup op a


class    (Semigroup op a, Commutative op a) => CommutativeSemigroup op a
instance (Semigroup op a, Commutative op a) => CommutativeSemigroup op a

class    (Semigroup op a, Neutral op a) => Monoid op a
instance (Semigroup op a, Neutral op a) => Monoid op a

class    (Monoid op a, Commutative op a) => CommutativeMonoid op a
instance (Monoid op a, Commutative op a) => CommutativeMonoid op a

class    (Monoid op a, Invertible op a) => Group op a
instance (Monoid op a, Invertible op a) => Group op a

class    (Group op a, Commutative op a) => AbelianGroup op a
instance (Group op a, Commutative op a) => AbelianGroup op a

-- | Two binary operations

-- | Semirings, aka "rigs"
class ( CommutativeMonoid add a
      , Monoid mul a
      , DistributesOver add mul a
      ) =>
      Semiring add mul a
instance ( CommutativeMonoid add a
         , Monoid mul a
         , DistributesOver add mul a
         ) =>
         Semiring add mul a
type Semiring' = Semiring Add Mul

class    (Semiring add mul a, Commutative mul a) => CommutativeSemiring add mul a
instance (Semiring add mul a, Commutative mul a) => CommutativeSemiring add mul a
type CommutativeSemiring' = CommutativeSemiring Add Mul

class ( AbelianGroup add a
      , Monoid mul a
      , DistributesOver add mul a
      ) => Ring add mul a

instance ( AbelianGroup add a
         , Monoid mul a
         , DistributesOver add mul a
         ) => Ring add mul a
type Ring' = Ring Add Mul

class    (Ring add mul a, Commutative mul a) => CommutativeRing add mul a
instance (Ring add mul a, Commutative mul a) => CommutativeRing add mul a
type CommutativeRing' = CommutativeRing Add Mul

class    (Ring add mul a, Invertible mul a) => DivisionRing add mul a
instance (Ring add mul a, Invertible mul a) => DivisionRing add mul a
type DivisionRing' = DivisionRing Add Mul

class    (Ring add mul a, AbelianGroup mul a) => Field add mul a
instance (Ring add mul a, AbelianGroup mul a) => Field add mul a
type Field' = Field Add Mul

-- instance (DivisionRing add mul a, Commutative mul a) => Field add mul a
-- works too

infixl 6 +

(+) :: Semigroup Add a => a -> a -> a
(+) = binaryOp (undefined :: Proxy Add)

infixl 7 *

(*) :: Semigroup Mul a => a -> a -> a
(*) = binaryOp (undefined :: Proxy Mul)

-- :t zero
-- zero :: (Neutral 'Add a, Associative 'Add a) => a
-- why? ;)

-- tagProxy :: Tag -> Proxy

zero :: Monoid Add a => a
zero = neutral (undefined :: Proxy Add)

one :: Monoid Mul a => a
one = neutral (undefined :: Proxy Mul)

negate :: AbelianGroup Add a => a -> a
negate = invert (undefined :: Proxy Add)

reciprocal :: Group Mul a => a -> a
reciprocal = invert (undefined :: Proxy Mul)

infixl 6 -

(-) :: AbelianGroup Add a => a -> a -> a
a - b = a + (negate b)

infixl 7 /
(/) :: Group Mul a => a -> a -> a
a / b = a * (reciprocal b)
