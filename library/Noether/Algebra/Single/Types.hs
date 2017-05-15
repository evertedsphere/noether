{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Noether.Algebra.Single.Types where

import qualified Prelude                 as P

import           Data.Complex

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

data UnaryTag = Neg
  deriving Show

data BinaryTag = Add | Mul
  deriving Show

data Peano = Z | S Peano

class UnaryNeutralK (op :: UnaryTag) a s where
  unaryNeutralK :: Proxy op -> Proxy s -> a

class UnaryOpK (op :: UnaryTag) a s where
  unaryOpK :: Proxy op -> Proxy s -> a -> a

-- | Magma

type family MagmaS (op :: k) (a :: Type) = (r :: Type)
type Magma op a = MagmaK op a (MagmaS op a)

class MagmaK (op :: k) a s where
  binaryOpK :: Proxy op -> Proxy s -> a -> a -> a

-- | NeutralK

type family NeutralS (op :: k) (a :: Type) = (r :: Type)
type Neutral op a = (NeutralK op a (NeutralS op a))

class NeutralK (op :: k) a s where
  neutralK :: Proxy op -> Proxy s -> a

-- | Cancellative

class CancellativeK op a s where
  cancelK :: Proxy op -> Proxy s -> a -> a

type family CancellativeS (op :: k) (a :: Type) = (r :: Type)

type Cancellative (op :: k) a = CancellativeK op a (CancellativeS op a)

-- (FIXME: any dependencies?)

type family CommutativeS (op :: k) (a :: Type) = (r :: Type)

-- FIXME: move constraint into instance, derive...?
class (Magma op a) => CommutativeK (op :: k) a s

type Commutative op a = (CommutativeK op a (CommutativeS op a))

{-| Semigroup

 -}

type family SemigroupS (op :: k) (a :: Type) = (r :: Type)

class (Magma op a) => SemigroupK (op :: k) a s

type Semigroup op a = (SemigroupK op a (SemigroupS op a))

{-| Cancellative semigroups

 -}


type family CancellativeSemigroupS (op :: k) (a :: Type) = (r :: Type)

class (Semigroup op a, Cancellative op a) =>
      CancellativeSemigroupK (op :: k) a s

type CancellativeSemigroup op a = CancellativeSemigroupK op a (CancellativeSemigroupS op a)


{-| Monoids

-}

type family MonoidS (op :: k) (a :: Type) = (r :: Type)

class (Semigroup op a, Neutral op a) => MonoidK (op :: k) a s

type Monoid op a = MonoidK op a (MonoidS op a)

{-| Groups

-}

class (Monoid op a, Cancellative op a) => GroupK op a s

type family GroupS (op :: k) (a :: Type) = (r :: Type)

type Group op a = GroupK op a (GroupS op a)

type CommSemigroup op a = (Commutative op a, Semigroup op a)
type CommMonoid    op a = (Commutative op a, Monoid    op a)
type Abelian       op a = (Commutative op a, Group     op a)

data Composite (a :: k) (b :: k')

--

data DerivedFrom (a :: k)

data Prim

type instance MagmaS         (_ :: BinaryTag) Double = Prim
type instance NeutralS       (_ :: BinaryTag) Double = Prim
type instance CancellativeS  (_ :: BinaryTag) Double = Prim
type instance CommutativeS   (_ :: BinaryTag) Double = Prim

type instance SemigroupS     (_ :: BinaryTag) Double = Prim
type instance MonoidS        (_ :: BinaryTag) Double = Prim
type instance GroupS         (_ :: BinaryTag) Double = Prim

-- Double, additive structure

instance MagmaK         Add Double Prim where binaryOpK _ _ = (P.+)
instance NeutralK       Add Double Prim where neutralK  _ _ = 0
instance CancellativeK  Add Double Prim where cancelK   _ _ = P.negate
instance CommutativeK   Add Double Prim

instance SemigroupK     Add Double Prim
instance MonoidK        Add Double Prim
instance GroupK         Add Double Prim

-- Double, multiplicative structure

instance MagmaK         Mul Double Prim where binaryOpK _ _ = (P.*)
instance NeutralK       Mul Double Prim where neutralK  _ _ = 1
instance CancellativeK  Mul Double Prim where cancelK   _ _ = (1 P./)
instance CommutativeK   Mul Double Prim

instance SemigroupK     Mul Double Prim
instance MonoidK        Mul Double Prim
instance GroupK         Mul Double Prim

-- Integers

type instance MagmaS         (_ :: BinaryTag) Integer = Prim
type instance NeutralS       (_ :: BinaryTag) Integer = Prim
type instance CancellativeS        Add        Integer = Prim
type instance CommutativeS   (_ :: BinaryTag) Integer = Prim

type instance SemigroupS     (_ :: BinaryTag) Integer = Prim
type instance MonoidS        (_ :: BinaryTag) Integer = Prim
type instance GroupS               Add        Integer = Prim

-- Additive

instance MagmaK         Add Integer Prim where binaryOpK _ _ = (P.+)
instance NeutralK       Add Integer Prim where neutralK  _ _ = 0
instance CancellativeK  Add Integer Prim where cancelK   _ _ = P.negate
instance CommutativeK   Add Integer Prim

instance SemigroupK     Add Integer Prim
instance MonoidK        Add Integer Prim
instance GroupK         Add Integer Prim

-- Multiplicative

instance MagmaK         Mul Integer Prim where binaryOpK _ _ = (P.*)
instance NeutralK       Mul Integer Prim where neutralK  _ _ = 1
instance CommutativeK   Mul Integer Prim

instance SemigroupK     Mul Integer Prim
instance MonoidK        Mul Integer Prim

-- | Complex numbers with real/imag. parts represented by Double
type ComplexD = Complex Double

type instance MagmaS         (_ :: BinaryTag) ComplexD = Prim
type instance NeutralS       (_ :: BinaryTag) ComplexD = Prim
type instance CancellativeS  (_ :: BinaryTag) ComplexD = Prim
type instance CommutativeS   (_ :: BinaryTag) ComplexD = Prim

type instance SemigroupS     (_ :: BinaryTag) ComplexD = Prim
type instance MonoidS        (_ :: BinaryTag) ComplexD = Prim
type instance GroupS         (_ :: BinaryTag) ComplexD = Prim

-- ComplexD, additive structure

instance MagmaK         Add ComplexD Prim where binaryOpK _ _ = (P.+)
instance NeutralK       Add ComplexD Prim where neutralK  _ _ = 0
instance CancellativeK  Add ComplexD Prim where cancelK   _ _ = P.negate
instance CommutativeK   Add ComplexD Prim

instance SemigroupK     Add ComplexD Prim
instance MonoidK        Add ComplexD Prim
instance GroupK         Add ComplexD Prim

-- ComplexD, multiplicative structure

instance MagmaK         Mul ComplexD Prim where binaryOpK _ _ = (P.*)
instance NeutralK       Mul ComplexD Prim where neutralK  _ _ = 1
instance CancellativeK  Mul ComplexD Prim where cancelK   _ _ = (1 P./)
instance CommutativeK   Mul ComplexD Prim

instance SemigroupK     Mul ComplexD Prim
instance MonoidK        Mul ComplexD Prim
instance GroupK         Mul ComplexD Prim
