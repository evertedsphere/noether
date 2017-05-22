module Noether.Algebra.Vector.Unboxed where

import qualified Prelude                     as P

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Actions
import           Noether.Algebra.Linear
import           Noether.Algebra.Single
import           Noether.Algebra.Tags
import           Noether.Algebra.Vector.Tags

import qualified Data.Vector.Unboxed         as U

{-| UVector n v ≅ v^n for 'Unbox' types 'v'. -}

newtype UVector (n :: Nat) v =
  UVector (U.Vector v)
  deriving (Show)

{-| Lifting addition and multiplication coordinatewise (Hadamard?) -}

instance (U.Unbox v, MagmaK op v s) =>
         MagmaK op (UVector n v) (MagmaTagged UVectorLift s) where
  binaryOpK o _ (UVector x) (UVector y) = UVector (U.zipWith binop x y)
    where
      binop = binaryOpK o (Proxy :: Proxy s)

{-| Neutral elements for addition and multiplication. -}

instance (U.Unbox v, KnownNat n, NeutralK op v s) =>
         NeutralK op (UVector n v) (NeutralTagged UVectorLift s) where
  neutralK o _ = UVector (U.replicate count neutralValue)
    where
      count = P.fromIntegral (natVal (Proxy :: Proxy n))
      neutralValue = neutralK o (Proxy :: Proxy s)

{-| Pointwise negation and inversion.

    Note that v^n has (a lot of) nontrivial zerodivisors even if v does not.
    The zerodivisors are all elements with a zero(divisor) in some coordinate,
    e.g. (1,0) and (0,1) are zerodivisors in R^2.

    (This corresponds to the idea that the Spec of a product ring is disconnected!)
-}

instance (U.Unbox v, KnownNat n, CancellativeK op v s) =>
         CancellativeK op (UVector n v) (CancellativeTagged UVectorLift s) where
  cancelK o _ (UVector vs) = UVector (U.map cancelK' vs)
    where
      cancelK' = cancelK o (Proxy :: Proxy s)

{-| Actions of a on b extend to actions of a on 'UVector n b'. -}

instance (U.Unbox b, KnownNat n, ActsK lr op a b s) =>
         ActsK lr op a (UVector n b) (ActsTagged UVectorLift s) where
  actK o _ lr a (UVector bs) = UVector (U.map (actK' a) bs)
    where
      actK' = actK o (Proxy :: Proxy s) lr

{- Instances of the "basic types". Everything else can be derived from these.
   We're simply choosing the strategies we defined above, using the Derive*
   synonyms to ease typing.
-}

type instance MagmaS        (op :: BinaryNumeric) (UVector n a) = DeriveMagma_Tagged        UVectorLift op a
type instance NeutralS      (op :: BinaryNumeric) (UVector n a) = DeriveNeutral_Tagged      UVectorLift op a
type instance CommutativeS  (op :: BinaryNumeric) (UVector n a) = DeriveCommutative_Tagged  UVectorLift op a

-- Protecting the innocent from zerodivisors since 1998

type instance CancellativeS  Add (UVector n a) = DeriveCancellative_Tagged UVectorLift Add a

{- Like I said: -}

type instance SemigroupS (op :: BinaryNumeric) (UVector n a) = DeriveSemigroup_Magma          op (UVector n a)
type instance MonoidS    (op :: BinaryNumeric) (UVector n a) = DeriveMonoid_Semigroup_Neutral op (UVector n a)

type instance GroupS        Add (UVector n a)  = DeriveGroup_Monoid_Cancellative      Add (UVector n a)
type instance AbelianGroupS Add (UVector n a)  = DeriveAbelianGroup_Commutative_Group Add (UVector n a)

type instance ActsS       lr Mul     a (UVector n b) = DeriveActs_Tagged UVectorLift   lr Mul a b
type instance CompatibleS lr Mul Mul a (UVector n b) = DeriveCompatible_Acts_Semigroup lr Mul Mul a (UVector n b)

type instance ActorLinearS lr Mul Add a Add (UVector n a) =
     DeriveActorLinearActs_Acts_Semigroup_Semigroup lr Mul Add a Add (UVector n a)
type instance ActeeLinearS lr Mul a Add (UVector n a) =
     DeriveActeeLinearActs_Acts_Semigroup lr Mul a Add (UVector n a)

v :: UVector 10 Double
v = UVector $ U.fromList [1..10]

w :: UVector 10 Double
w = UVector $ U.fromList [10,9..1]

-- Inferred type:
-- f ::
--   (MagmaK 'Mul a (MagmaS 'Mul a), MagmaK 'Add a (MagmaS 'Add a),
--    CancellativeK 'Add a (CancellativeS 'Add a)) =>
--   a -> a

f x = x + x - x * x

-- | This is equal to
-- > UVector [5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0]

g :: [UVector 10 Double]
g = map (\x -> lerp x v w) [0.0,0.1..1.0 :: Double]
