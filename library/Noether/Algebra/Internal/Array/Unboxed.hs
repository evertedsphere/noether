module Noether.Algebra.Internal.Array.Unboxed where

import qualified Prelude                             as P

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Internal.Array.Tags
import           Noether.Algebra.Single
import           Noether.Algebra.Tags

import           Data.Vector.Unboxed                 as U

newtype UArray (n :: Nat) v =
  UArray (U.Vector v)
  deriving (Show)

instance (Unbox v, MagmaK op v s) =>
         MagmaK op (UArray n v) (MagmaTagged UVectorLift s) where
  binaryOpK o _ (UArray x) (UArray y) = UArray (U.zipWith binop x y)
    where
      binop = binaryOpK o (Proxy :: Proxy s)

instance (Unbox v, KnownNat n, NeutralK op v s) =>
         NeutralK op (UArray n v) (NeutralTagged UVectorLift s) where
  neutralK o _ = UArray (U.replicate count neutralValue)
    where
      count = P.fromIntegral (natVal (Proxy :: Proxy n))
      neutralValue = neutralK o (Proxy :: Proxy s)

instance (Unbox v, KnownNat n, CancellativeK op v s) =>
         CancellativeK op (UArray n v) (CancellativeTagged UVectorLift s) where
  cancelK o _ (UArray vs) = UArray (U.map cancelK' vs)
    where
      cancelK' = cancelK o (Proxy :: Proxy s)

type instance MagmaS (op :: BinaryNumeric) (UArray n Double) =
     DeriveMagma_Tagged UVectorLift op Double

type instance NeutralS (op :: BinaryNumeric) (UArray n Double) =
     DeriveNeutral_Tagged UVectorLift op Double

type instance CommutativeS (op :: BinaryNumeric) (UArray n Double) =
     DeriveCommutative_Tagged UVectorLift op Double

type instance CancellativeS (op :: BinaryNumeric) (UArray n Double) =
     DeriveCancellative_Tagged UVectorLift op Double

type instance SemigroupS (op :: BinaryNumeric) (UArray n Double) =
     DeriveSemigroup_Magma op (UArray n Double)

type instance MonoidS (op :: BinaryNumeric) (UArray n Double) =
     DeriveMonoid_Semigroup_Neutral op (UArray n Double)

type instance GroupS (op :: BinaryNumeric) (UArray n Double) =
     DeriveGroup_Monoid_Cancellative op (UArray n Double)

type instance AbelianGroupS (op :: BinaryNumeric) (UArray n Double) =
     DeriveAbelianGroup_Commutative_Group op (UArray n Double)

vec :: UArray 10 Double
vec = zero + one * one

-- Inferred type:
-- f ::
--   (CancellativeK 'Mul a (CancellativeS 'Mul a),
--    CancellativeK 'Add a (CancellativeS 'Add a),
--    MagmaK 'Mul a (MagmaS 'Mul a), MagmaK 'Add a (MagmaS 'Add a)) =>
--   a -> a
-- basically, something ringish
f x = x + x - x / x

g :: UArray 10 Double
g = f vec
