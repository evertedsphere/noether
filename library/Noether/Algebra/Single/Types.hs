{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Noether.Algebra.Single.Types where

import qualified Prelude                    as P

import           Data.Complex

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu
import           Noether.Lemmata.TypeFu.Map

data UnaryTag = Neg deriving Show
data BinaryTag = Add | Mul deriving Show

data Peano = Z | S Peano

class UnaryNeutralK (op :: UnaryTag) a s where
  unaryNeutralK :: Proxy op -> Proxy s -> a

class UnaryOpK (op :: UnaryTag) a s where
  unaryOpK :: Proxy op -> Proxy s -> a -> a

type family MagmaS        (op :: k) (a :: Type) = (r :: Type)
type family NeutralS      (op :: k) (a :: Type) = (r :: Type)
type family CancellativeS (op :: k) (a :: Type) = (r :: Type)
type family CommutativeS  (op :: k) (a :: Type) = (r :: Type)
type family SemigroupS    (op :: k) (a :: Type) = (r :: Type)
type family MonoidS       (op :: k) (a :: Type) = (r :: Type)
type family GroupS        (op :: k) (a :: Type) = (r :: Type)

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

class MagmaK (op :: k) a s where
  binaryOpK :: Proxy op -> Proxy s -> a -> a -> a

class NeutralK (op :: k) a s where
  neutralK :: Proxy op -> Proxy s -> a

class CancellativeK (op :: k) a s where
  cancelK :: Proxy op -> Proxy s -> a -> a

class CommutativeK (op :: k) a s

class SemigroupK (op :: k) a s

class MonoidK (op :: k) a s

class GroupK (op :: k) a s

-- bahahahaha
data Synergise (a :: [k])

instance ( SemigroupK op a zs
         , NeutralK op a zn
         ) => MonoidK op a (Synergise '["Neutral" := zn, "Semigroup" := zs])

instance ( MonoidK op a zm
         , CancellativeK op a zc
         ) => GroupK op a (Synergise '["Cancellative" := zc, "Monoid" := zm])

instance (MagmaK op a zm) =>
         SemigroupK op a (Synergise '["Magma" := zm])

instance CommutativeK op a Prim

data DerivedFrom (a :: k)

data Prim

type family SymbolToFamily (t :: k) (a :: Type) (hint :: Symbol) :: Type

type instance SymbolToFamily t a "Magma" = MagmaS t a
type instance SymbolToFamily t a "Neutral" = NeutralS t a
type instance SymbolToFamily t a "Semigroup" = SemigroupS t a
type instance SymbolToFamily t a "Cancellative" = CancellativeS t a
type instance SymbolToFamily t a "Monoid" = MonoidS t a
type instance SymbolToFamily t a "Group" = GroupS t a

type family Infer_ (t :: k) (a :: Type) (hint :: [Symbol]) :: [Type] where
  Infer_ t a '[] = '[]
  Infer_ t a (x : xs) = (x := SymbolToFamily t a x) : Infer_ t a xs

type Infer (t :: k) a (h :: [Symbol]) = Synergise (Nub (Sort (Infer_ t a h)))

-- Double

type instance MagmaS         (_ :: BinaryTag) Double = Prim
type instance NeutralS       (_ :: BinaryTag) Double = Prim
type instance CommutativeS   (_ :: BinaryTag) Double = Prim
type instance CancellativeS  (_ :: BinaryTag) Double = Prim

type instance SemigroupS t Double = Infer t Double '["Magma"]
type instance MonoidS    t Double = Infer t Double '["Semigroup", "Neutral"]
type instance GroupS     t Double = Infer t Double '["Cancellative", "Monoid"]

instance MagmaK         Add Double Prim where binaryOpK _ _ = (P.+)
instance NeutralK       Add Double Prim where neutralK  _ _ = 0
instance CancellativeK  Add Double Prim where cancelK   _ _ = P.negate

instance MagmaK         Mul Double Prim where binaryOpK _ _ = (P.*)
instance NeutralK       Mul Double Prim where neutralK  _ _ = 1
instance CancellativeK  Mul Double Prim where cancelK   _ _ = (1 P./)

-- Integers

type instance MagmaS         (_ :: BinaryTag) Integer = Prim
type instance NeutralS       (_ :: BinaryTag) Integer = Prim
type instance CommutativeS   (_ :: BinaryTag) Integer = Prim
type instance CancellativeS        Add        Integer = Prim

type instance SemigroupS t Integer = Infer t Integer '["Magma"]
type instance MonoidS    t Integer = Infer t Integer '["Semigroup", "Neutral"]
type instance GroupS   Add Integer = Infer Add Integer '["Cancellative", "Monoid"]

instance MagmaK         Add Integer Prim where binaryOpK _ _ = (P.+)
instance NeutralK       Add Integer Prim where neutralK  _ _ = 0
instance CancellativeK  Add Integer Prim where cancelK   _ _ = P.negate

instance MagmaK         Mul Integer Prim where binaryOpK _ _ = (P.*)
instance NeutralK       Mul Integer Prim where neutralK  _ _ = 1

-- | Complex numbers with real/imag. parts represented by Double

type ComplexD = Complex Double

type instance MagmaS         (_ :: BinaryTag) ComplexD = Prim
type instance NeutralS       (_ :: BinaryTag) ComplexD = Prim
type instance CommutativeS   (_ :: BinaryTag) ComplexD = Prim
type instance CancellativeS  (_ :: BinaryTag) ComplexD = Prim

type instance SemigroupS t ComplexD = Infer t ComplexD '["Magma"]
type instance MonoidS    t ComplexD = Infer t ComplexD '["Semigroup", "Neutral"]
type instance GroupS     t ComplexD = Infer t ComplexD '["Cancellative", "Monoid"]

instance MagmaK         Add ComplexD Prim where binaryOpK _ _ = (P.+)
instance NeutralK       Add ComplexD Prim where neutralK  _ _ = 0
instance CancellativeK  Add ComplexD Prim where cancelK   _ _ = P.negate

instance MagmaK         Mul ComplexD Prim where binaryOpK _ _ = (P.*)
instance NeutralK       Mul ComplexD Prim where neutralK  _ _ = 1
instance CancellativeK  Mul ComplexD Prim where cancelK   _ _ = (1 P./)
