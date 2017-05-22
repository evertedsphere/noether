module Noether.Algebra.Internal.Array.Boxed where

import qualified Prelude                             as P

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Actions
import           Noether.Algebra.Internal.Array.Tags
import           Noether.Algebra.Linear
import           Noether.Algebra.Single
import           Noether.Algebra.Tags

import qualified Data.Vector                         as V

{-| BVector n v ≅ v^n for 'Unbox' types 'v'. -}

newtype BVector (n :: Nat) v =
  BVector (V.Vector v)
  deriving (Show)

unsafeFromList :: [a] -> BVector n a
unsafeFromList as = BVector (V.fromList as)

{-| Lifting addition and multiplication coordinatewise (Hadamard?) -}

instance (MagmaK op v s) =>
         MagmaK op (BVector n v) (MagmaTagged BVectorLift s) where
  binaryOpK o _ (BVector x) (BVector y) = BVector (V.zipWith binop x y)
    where
      binop = binaryOpK o (Proxy :: Proxy s)

{-| Neutral elements for addition and multiplication. -}

instance (KnownNat n, NeutralK op v s) =>
         NeutralK op (BVector n v) (NeutralTagged BVectorLift s) where
  neutralK o _ = BVector (V.replicate count neutralValue)
    where
      count = P.fromIntegral (natVal (Proxy :: Proxy n))
      neutralValue = neutralK o (Proxy :: Proxy s)

{-| Pointwise negation and inversion.

    Note that v^n has (a lot of) nontrivial zerodivisors even if v does not.
    The zerodivisors are all elements with a zero(divisor) in some coordinate,
    e.g. (1,0) and (0,1) are zerodivisors in R^2.

    (This corresponds to the idea that the Spec of a product ring is disconnected!)
-}

instance (KnownNat n, CancellativeK op v s) =>
         CancellativeK op (BVector n v) (CancellativeTagged BVectorLift s) where
  cancelK o _ (BVector vs) = BVector (V.map cancelK' vs)
    where
      cancelK' = cancelK o (Proxy :: Proxy s)

{-| Actions of a on b extend to actions of a on 'BVector n b'. -}

instance (KnownNat n, ActsK lr op a b s) =>
         ActsK lr op a (BVector n b) (ActsTagged BVectorLift s) where
  actK o _ lr a (BVector bs) = BVector (V.map (actK' a) bs)
    where
      actK' = actK o (Proxy :: Proxy s) lr

{- Instances of the "basic types". Everything else can be derived from these.
   We're simply choosing the strategies we defined above, using the Derive*
   synonyms to ease typing.
-}

type instance MagmaS        (op :: BinaryNumeric) (BVector n a) = DeriveMagma_Tagged        BVectorLift op a
type instance NeutralS      (op :: BinaryNumeric) (BVector n a) = DeriveNeutral_Tagged      BVectorLift op a
type instance CommutativeS  (op :: BinaryNumeric) (BVector n a) = DeriveCommutative_Tagged  BVectorLift op a

-- Protecting the innocent from zerodivisors since 1998

type instance CancellativeS  Add (BVector n a) = DeriveCancellative_Tagged BVectorLift Add a

{- Like I said: -}

type instance SemigroupS (op :: BinaryNumeric) (BVector n a) = DeriveSemigroup_Magma          op (BVector n a)
type instance MonoidS    (op :: BinaryNumeric) (BVector n a) = DeriveMonoid_Semigroup_Neutral op (BVector n a)

type instance GroupS        Add (BVector n a)  = DeriveGroup_Monoid_Cancellative      Add (BVector n a)
type instance AbelianGroupS Add (BVector n a)  = DeriveAbelianGroup_Commutative_Group Add (BVector n a)

type instance ActsS       lr Mul     a (BVector n b) = DeriveActs_Tagged BVectorLift   lr Mul a b
type instance CompatibleS lr Mul Mul a (BVector n b) = DeriveCompatible_Acts_Semigroup lr Mul Mul a (BVector n b)

type instance ActorLinearS lr Mul Add a Add (BVector n a) =
     DeriveActorLinearActs_Acts_Semigroup_Semigroup lr Mul Add a Add (BVector n a)
type instance ActeeLinearS lr Mul a Add (BVector n a) =
     DeriveActeeLinearActs_Acts_Semigroup lr Mul a Add (BVector n a)

v :: BVector 10 Rational
v = unsafeFromList [1..10]

w :: BVector 10 Rational
w = unsafeFromList [10,9..1]

-- Inferred type:
-- f ::
--   (MagmaK 'Mul a (MagmaS 'Mul a), MagmaK 'Add a (MagmaS 'Add a),
--    CancellativeK 'Add a (CancellativeS 'Add a)) =>
--   a -> a

f x = x + x - x * x

-- | This is equal to
-- > BVector [5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0]

g :: [BVector 10 Rational]
g = map (\γ -> lerp γ v w) [0.0,0.1..1.0 :: Rational]
