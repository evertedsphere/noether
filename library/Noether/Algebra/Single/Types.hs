{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Noether.Algebra.Single.Types where

import qualified Prelude                 as P

import           Data.Complex

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

data UnaryTag = Neg deriving Show
data BinaryTag = Add | Mul deriving Show

data Peano = Z | S Peano

class UnaryNeutralK (op :: UnaryTag) a (s :: k') where
  unaryNeutralK :: Proxy op -> Proxy s -> a

class UnaryOpK (op :: UnaryTag) a (s :: k') where
  unaryOpK :: Proxy op -> Proxy s -> a -> a

type family MagmaS        (op :: k) (a :: Type) = (r :: Type)
type family NeutralS      (op :: k) (a :: Type) = (r :: Type)
type family CancellativeS (op :: k) (a :: Type) = (r :: Type)
type family CommutativeS  (op :: k) (a :: Type) = (r :: Type)
type family SemigroupS    (op :: k) (a :: Type) = (r :: Type)
type family MonoidS       (op :: k) (a :: Type) = (r :: Type)
type family GroupS        (op :: k) (a :: Type) = (r :: Type)

infixl 6 &.
type (&.) (a :: k -> k' -> Constraint) (b :: k -> k' -> Constraint) (p :: k) (q :: k')
  = (a p q, b p q)

infixl 7 $>
type ($>) (a :: k1 -> k2 -> k3 -> k4) (b :: k1 -> k2 -> k3) (p :: k1) (q :: k2)
  = a p q (b p q)

type Neutral op a = (NeutralK $> NeutralS) op a
type Commutative op a = (CommutativeK $> CommutativeS) op a
type Cancellative op a = (CancellativeK $> CancellativeS) op a

type Magma     op a = (MagmaK $> MagmaS) op a
type Semigroup op a = (SemigroupK $> SemigroupS &. Magma) op a
type Monoid    op a = (MonoidK $> MonoidS &. Neutral &. Semigroup) op a
type Group     op a = (GroupK $> GroupS &. Monoid &. Cancellative) op a

type CancellativeSemigroup op a = (Cancellative &. Semigroup) op a
type CommSemigroup op a = (Commutative &. Semigroup) op a
type CommMonoid op a = (Commutative &. Monoid) op a
type Abelian op a = (Commutative &. Group) op a

class MagmaK (op :: k) a (s :: k') where
  binaryOpK :: Proxy op -> Proxy s -> a -> a -> a

class NeutralK (op :: k) a (s :: k') where
  neutralK :: Proxy op -> Proxy s -> a

class CancellativeK (op :: k) a (s :: k') where
  cancelK :: Proxy op -> Proxy s -> a -> a

class CommutativeK (op :: k) a (s :: k')

class SemigroupK (op :: k) a (s :: k')

class MonoidK (op :: k) a (s :: k')

class GroupK (op :: k) a (s :: k')

data Composite (a :: [k])

data (:=) (sym :: k) (t :: k')

instance (SemigroupK op a zs, NeutralK op a zn) =>
         MonoidK op a (Composite [SemigroupK := zs, NeutralK := zn])

instance (MonoidK op a zm, CancellativeK op a zc) =>
         GroupK op a (Composite [MonoidK := zm, CancellativeK := zc])

instance SemigroupK   op a Prim
instance CommutativeK op a Prim

data DerivedFrom (a :: k)

data Prim

type family Infer (c :: k1) (t :: k2) (a :: Type) :: Type where
  Infer MonoidK t a = Composite [SemigroupK := SemigroupS t a, NeutralK := NeutralS t a]
  Infer GroupK  t a = Composite [MonoidK := MonoidS t a, CancellativeK := CancellativeS t a]

-- Double

type instance MagmaS         (_ :: BinaryTag) Double = Prim
type instance NeutralS       (_ :: BinaryTag) Double = Prim
type instance CancellativeS  (_ :: BinaryTag) Double = Prim
type instance CommutativeS   (_ :: BinaryTag) Double = Prim

type instance SemigroupS     (_ :: BinaryTag) Double = Prim

type instance MonoidS t Double = Infer MonoidK t Double
type instance GroupS  t Double = Infer GroupK  t Double

instance MagmaK         Add Double Prim where binaryOpK _ _ = (P.+)
instance NeutralK       Add Double Prim where neutralK  _ _ = 0
instance CancellativeK  Add Double Prim where cancelK   _ _ = P.negate

instance MagmaK         Mul Double Prim where binaryOpK _ _ = (P.*)
instance NeutralK       Mul Double Prim where neutralK  _ _ = 1
instance CancellativeK  Mul Double Prim where cancelK   _ _ = (1 P./)

-- Integers

type instance MagmaS         (_ :: BinaryTag) Integer = Prim
type instance NeutralS       (_ :: BinaryTag) Integer = Prim
type instance CancellativeS        Add        Integer = Prim
type instance CommutativeS   (_ :: BinaryTag) Integer = Prim

type instance SemigroupS     (_ :: BinaryTag) Integer = Prim

type instance MonoidS t Integer = Infer MonoidK t Integer
type instance GroupS Add Integer = Infer GroupK Add Integer

instance MagmaK         Add Integer Prim where binaryOpK _ _ = (P.+)
instance NeutralK       Add Integer Prim where neutralK  _ _ = 0
instance CancellativeK  Add Integer Prim where cancelK   _ _ = P.negate

instance MagmaK         Mul Integer Prim where binaryOpK _ _ = (P.*)
instance NeutralK       Mul Integer Prim where neutralK  _ _ = 1

-- | Complex numbers with real/imag. parts represented by Double

type ComplexD = Complex Double

type instance MagmaS         (_ :: BinaryTag) ComplexD = Prim
type instance NeutralS       (_ :: BinaryTag) ComplexD = Prim
type instance CancellativeS  (_ :: BinaryTag) ComplexD = Prim
type instance CommutativeS   (_ :: BinaryTag) ComplexD = Prim
type instance SemigroupS     (_ :: BinaryTag) ComplexD = Prim

type instance MonoidS t ComplexD = Infer MonoidK t ComplexD
type instance GroupS  t ComplexD = Infer GroupK  t ComplexD

instance MagmaK         Add ComplexD Prim where binaryOpK _ _ = (P.+)
instance NeutralK       Add ComplexD Prim where neutralK  _ _ = 0
instance CancellativeK  Add ComplexD Prim where cancelK   _ _ = P.negate

instance MagmaK         Mul ComplexD Prim where binaryOpK _ _ = (P.*)
instance NeutralK       Mul ComplexD Prim where neutralK  _ _ = 1
instance CancellativeK  Mul ComplexD Prim where cancelK   _ _ = (1 P./)
