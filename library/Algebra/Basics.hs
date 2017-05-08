{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Algebra.Basics where

import           Data.Proxy
import           GHC.Exts

import           Prelude      hiding (Monoid, fromInteger, negate, recip, (*),
                               (+), (-), (/))
import qualified Prelude      as P

import           Data.Complex

data UnaryTag = Neg
  deriving Show

data BinaryTag = Add | Mul
  deriving Show

pattern AddP :: Proxy Add
pattern AddP = Proxy

pattern MulP :: Proxy Mul
pattern MulP = Proxy

fromInteger :: Num a => Integer -> a
fromInteger = P.fromInteger

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

-- | Classes

-- | Single associative binary operation

-- FIXME: any dependencies?
class (Magma op a) => Commutative op a
class (Magma op a) => Semigroup op a

-- class    (Semigroup op a, Invertible op a) => CancellativeSemigroup op a

class (Semigroup op a, Commutative op a) => CommSemigroup op a

class (Semigroup op a, Neutral op a)     => Monoid op a
class (Monoid op a,    Commutative op a) => CommMonoid op a

class (Monoid op a,    Invertible op a)  => Group op a
class (Group op a,     Commutative op a) => AbelianGroup op a

-- Two binary operations

-- | Semirings, aka "rigs"
class ( CommMonoid add a
      , Monoid mul a
      , DistributesOver add mul a
      ) => Semiring add mul a
class (Semiring add mul a, Commutative mul a) => CommSemiring add mul a

class ( AbelianGroup add a
      , Monoid mul a
      , DistributesOver add mul a
      ) => Ring add mul a
class (Ring add mul a, Commutative mul a) => CommRing add mul a

class (Ring add mul a, Invertible mul a) => DivisionRing add mul a
class (Ring add mul a, AbelianGroup mul a) => Field add mul a

-- Convenience synonyms

type Semiring'            = Semiring            Add Mul
type CommSemiring' = CommSemiring Add Mul
type Ring'                = Ring                Add Mul
type CommRing'     = CommRing     Add Mul
type DivisionRing'        = DivisionRing        Add Mul
type Field'               = Field               Add Mul

-- | Instances

instance (Semigroup op a, Commutative op a) => CommSemigroup op a

instance (Semigroup op a, Neutral op a)  => Monoid op a
instance (Monoid op a, Commutative op a) => CommMonoid op a

instance (Monoid op a, Invertible op a) => Group op a
instance (Group op a, Commutative op a) => AbelianGroup op a

instance ( CommMonoid add a
         , Monoid mul a
         , DistributesOver add mul a
         ) => Semiring add mul a

instance (Semiring add mul a, Commutative mul a) => CommSemiring add mul a

instance ( AbelianGroup add a
         , Monoid mul a
         , DistributesOver add mul a
         ) => Ring add mul a

instance (Ring add mul a, Commutative mul a) => CommRing add mul a

instance (Ring add mul a, Invertible mul a) => DivisionRing add mul a
instance (Ring add mul a, AbelianGroup mul a) => Field add mul a

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

-- | Complex numbers with real/imag. parts represented by Double
type ComplexD = Complex Double

instance Neutral Add ComplexD where neutral _ = fromInteger 0
instance Magma Add ComplexD where binaryOp _ = (P.+)
instance Invertible Add ComplexD where invert _ = P.negate
instance Commutative Add ComplexD
instance Semigroup Add ComplexD
instance DistributesOver Add Mul ComplexD

instance Neutral Mul ComplexD where neutral _ = fromInteger 1
instance Magma Mul ComplexD where binaryOp _ = (P.*)
instance Invertible Mul ComplexD where invert _ = (P./ fromInteger 1)
instance Commutative Mul ComplexD
instance Semigroup Mul ComplexD

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

instance Neutral o a =>
         Neutral o (i -> a) where
  neutral proxy _ = neutral proxy

instance Magma o a =>
         Magma o (i -> a) where
  binaryOp proxy f g = \x -> binaryOp proxy (f x) (g x)

instance Invertible o a =>
         Invertible o (i -> a) where
  invert proxy f = \x -> invert proxy (f x)

instance Semigroup o a =>
         Semigroup o (i -> a)

instance Commutative o a =>
         Commutative o (i -> a)

instance DistributesOver p m a =>
         DistributesOver p m (i -> a)

instance (Neutral o a, Neutral o b) =>
         Neutral o (a, b) where
  neutral = \p -> (neutral p, neutral p)

instance (Magma o a, Magma o b) =>
         Magma o (a, b) where
  binaryOp proxy (a, b) (a', b') = (binaryOp proxy a a', binaryOp proxy b b')

instance (Invertible o a, Invertible o b) =>
         Invertible o (a, b) where
  invert proxy (a, b) = (invert proxy a, invert proxy b)

instance (Semigroup o a, Semigroup o b) =>
         Semigroup o (a, b)

instance (Commutative o a, Commutative o b) =>
         Commutative o (a, b)

instance (DistributesOver p m a, DistributesOver p m b) =>
         DistributesOver p m (a, b)


-- instance Group o a => Group o (i -> a)

infixl 6 +

(+) :: Semigroup Add a => a -> a -> a
(+) = binaryOp AddP

infixl 7 *

(*) :: Semigroup Mul a => a -> a -> a
(*) = binaryOp MulP

zero :: Monoid Add a => a
zero = neutral AddP

one :: Monoid Mul a => a
one = neutral MulP

negate :: AbelianGroup Add a => a -> a
negate = invert AddP

reciprocal :: Group Mul a => a -> a
reciprocal = invert MulP

infixl 6 -

(-) :: AbelianGroup Add a => a -> a -> a
a - b = a + (negate b)

infixl 7 /
(/) :: Group Mul a => a -> a -> a
a / b = a * (reciprocal b)
