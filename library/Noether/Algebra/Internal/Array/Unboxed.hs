module Noether.Algebra.Internal.Array.Unboxed where

import qualified Prelude                             as P

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Actions
import           Noether.Algebra.Internal.Array.Tags
import           Noether.Algebra.Linear
import           Noether.Algebra.Single
import           Noether.Algebra.Tags

import           Data.Vector.Unboxed                 as U

{-| UArray n v ≅ v^n for 'Unbox' types 'v'. -}

newtype UArray (n :: Nat) v =
  UArray (U.Vector v)
  deriving (Show)

{-| Lifting addition and multiplication coordinatewise (Hadamard?) -}

instance (Unbox v, MagmaK op v s) =>
         MagmaK op (UArray n v) (MagmaTagged UVectorLift s) where
  binaryOpK o _ (UArray x) (UArray y) = UArray (U.zipWith binop x y)
    where
      binop = binaryOpK o (Proxy :: Proxy s)

{-| Neutral elements for addition and multiplication. -}

instance (Unbox v, KnownNat n, NeutralK op v s) =>
         NeutralK op (UArray n v) (NeutralTagged UVectorLift s) where
  neutralK o _ = UArray (U.replicate count neutralValue)
    where
      count = P.fromIntegral (natVal (Proxy :: Proxy n))
      neutralValue = neutralK o (Proxy :: Proxy s)

{-| Pointwise negation and inversion.

    Note that v^n has (a lot of) nontrivial zerodivisors even if v does not.

    The zerodivisors are all elements with a zero(divisor) in some coordinate,
    e.g. (1,0) and (0,1) are zerodivisors in R^2.

    (This corresponds to the idea that the Spec of a product ring is disconnected!)
-}

instance (Unbox v, KnownNat n, CancellativeK op v s) =>
         CancellativeK op (UArray n v) (CancellativeTagged UVectorLift s) where
  cancelK o _ (UArray vs) = UArray (U.map cancelK' vs)
    where
      cancelK' = cancelK o (Proxy :: Proxy s)

{-| Actions of a on b extend to actions of a on 'UArray n b'. -}

instance (Unbox b, KnownNat n, ActsK lr op a b s) =>
         ActsK lr op a (UArray n b) (ActsTagged UVectorLift s) where
  actK o _ lr a (UArray bs) = UArray (U.map (actK' a) bs)
    where
      actK' = actK o (Proxy :: Proxy s) lr

{- Instances of the "basic types". Everything else can be derived from these.
   We're simply choosing the strategies we defined above, using the Derive*
   synonyms to ease typing.
-}

type instance MagmaS        (op :: BinaryNumeric) (UArray n a) = DeriveMagma_Tagged        UVectorLift op a
type instance NeutralS      (op :: BinaryNumeric) (UArray n a) = DeriveNeutral_Tagged      UVectorLift op a
type instance CommutativeS  (op :: BinaryNumeric) (UArray n a) = DeriveCommutative_Tagged  UVectorLift op a

{- Protecting the innocent from zerodivisors since 1998 -}

type instance CancellativeS  Add (UArray n a) = DeriveCancellative_Tagged UVectorLift Add a

{- Like I said: -}

type instance SemigroupS (op :: BinaryNumeric) (UArray n a) = DeriveSemigroup_Magma          op (UArray n a)
type instance MonoidS    (op :: BinaryNumeric) (UArray n a) = DeriveMonoid_Semigroup_Neutral op (UArray n a)

type instance GroupS        Add (UArray n a)                = DeriveGroup_Monoid_Cancellative      Add (UArray n a)
type instance AbelianGroupS Add (UArray n a)                = DeriveAbelianGroup_Commutative_Group Add (UArray n a)

type instance CompatibleS lr Mul Mul a (UArray n b)         = DeriveCompatible_Acts_Semigroup lr Mul Mul a (UArray n b)
type instance ActsS       lr Mul     a (UArray n b)         = DeriveActs_Tagged UVectorLift   lr Mul a b

type instance ActorLinearS lr Mul Add a Add (UArray n a) =
     DeriveActorLinearActs_Acts_Semigroup_Semigroup lr Mul Add a Add (UArray n a)
type instance ActeeLinearS lr Mul a Add (UArray n a) =
     DeriveActeeLinearActs_Acts_Semigroup lr Mul a Add (UArray n a)

vec :: UArray 10 Double
vec = zero + one * one + one + one

-- Inferred type:
-- f ::
--   (MagmaK 'Mul a (MagmaS 'Mul a), MagmaK 'Add a (MagmaS 'Add a),
--    CancellativeK 'Add a (CancellativeS 'Add a)) =>
--   a -> a

f x = x + x - x * x

-- | This is equal to
-- > UArray [5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0]

g :: UArray 10 Double
g = vec + (2 :: Double) %< vec
