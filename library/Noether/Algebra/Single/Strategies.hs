module Noether.Algebra.Single.Strategies where

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Tags

import           Noether.Algebra.Single.Cancellative
import           Noether.Algebra.Single.Commutative
import           Noether.Algebra.Single.Neutral

import           Noether.Algebra.Single.Magma
import           Noether.Algebra.Single.Monoid
import           Noether.Algebra.Single.Semigroup

import           Noether.Algebra.Single.AbelianGroup
import           Noether.Algebra.Single.Group

type Semigroup op a = (SemigroupC op a, Magma op a)

type Monoid op a = (MonoidC op a, Semigroup op a, Neutral op a)

type Group op a = (GroupC op a, Monoid op a, Cancellative op a)

type AbelianGroup op a = (AbelianGroupC op a, Group op a, Commutative op a)

-- Lifting strategies

type DeriveMagma_Tagged tag op a = MagmaTagged tag (MagmaS op a)
type DeriveMagma_Named tag op a = MagmaNamed tag (MagmaS op a)
type DeriveCommutative_Tagged tag op a = CommutativeTagged tag (CommutativeS op a)
type DeriveCancellative_Tagged tag op a = CancellativeTagged tag (CancellativeS op a)
type DeriveNeutral_Tagged tag op a = NeutralTagged tag (NeutralS op a)
type DeriveSemigroup_Magma (t :: k) (a :: Type) = Semigroup_Magma (MagmaS t a)

type DeriveMonoid_Semigroup_Neutral t a =
  Monoid_Semigroup_Neutral (SemigroupS t a) (NeutralS t a)

type DeriveGroup_Monoid_Cancellative t a =
  Group_Monoid_Cancellative (MonoidS t a) (CancellativeS t a)

type DeriveAbelianGroup_Commutative_Group t a =
  AbelianGroup_Commutative_Group (CommutativeS t a) (GroupS t a)

type DeriveAbelianGroup_Commutative_Monoid_Cancellative t a =
  AbelianGroup_Commutative_Group
    (CommutativeS t a)
    (DeriveGroup_Monoid_Cancellative t a)

-- Instances

type instance MagmaS (_ :: BinaryBoolean) Bool = MagmaPrim
type instance NeutralS (_ :: BinaryBoolean) Bool = NeutralPrim
type instance CommutativeS (_ :: BinaryBoolean) Bool = CommutativeNum
type instance SemigroupS (op :: BinaryBoolean) Bool =
     DeriveSemigroup_Magma op Bool

-- Int

type instance MagmaS       (_ :: BinaryNumeric) Int = MagmaNum
type instance NeutralS     (_ :: BinaryNumeric) Int = NeutralNum
type instance CommutativeS (_ :: BinaryNumeric) Int = CommutativeNum

type instance SemigroupS  (op :: BinaryNumeric) Int = DeriveSemigroup_Magma          op Int
type instance MonoidS     (op :: BinaryNumeric) Int = DeriveMonoid_Semigroup_Neutral op Int

type instance CancellativeS Add Int = CancellativeNum
type instance GroupS        Add Int = DeriveGroup_Monoid_Cancellative      Add Int
type instance AbelianGroupS Add Int = DeriveAbelianGroup_Commutative_Group Add Int

-- Integer

type instance MagmaS       (_ :: BinaryNumeric) Integer = MagmaNum
type instance NeutralS     (_ :: BinaryNumeric) Integer = NeutralNum
type instance CommutativeS (_ :: BinaryNumeric) Integer = CommutativeNum

type instance SemigroupS  (op :: BinaryNumeric) Integer = DeriveSemigroup_Magma          op Integer
type instance MonoidS     (op :: BinaryNumeric) Integer = DeriveMonoid_Semigroup_Neutral op Integer

type instance CancellativeS Add Integer = CancellativeNum
type instance GroupS        Add Integer = DeriveGroup_Monoid_Cancellative      Add Integer
type instance AbelianGroupS Add Integer = DeriveAbelianGroup_Commutative_Group Add Integer

-- Float

type instance MagmaS       (_ :: BinaryNumeric) Float = MagmaNum
type instance NeutralS     (_ :: BinaryNumeric) Float = NeutralNum
type instance CommutativeS (_ :: BinaryNumeric) Float = CommutativeNum

type instance SemigroupS  (op :: BinaryNumeric) Float = DeriveSemigroup_Magma          op Float
type instance MonoidS     (op :: BinaryNumeric) Float = DeriveMonoid_Semigroup_Neutral op Float

type instance CancellativeS Add Float = CancellativeNum
type instance CancellativeS Mul Float = CancellativeFractional

type instance GroupS        (op :: BinaryNumeric) Float = DeriveGroup_Monoid_Cancellative      op Float
type instance AbelianGroupS (op :: BinaryNumeric) Float = DeriveAbelianGroup_Commutative_Group op Float

-- Double

type instance MagmaS       (_ :: BinaryNumeric) Double = MagmaNum
type instance NeutralS     (_ :: BinaryNumeric) Double = NeutralNum
type instance CommutativeS (_ :: BinaryNumeric) Double = CommutativeNum

type instance SemigroupS  (op :: BinaryNumeric) Double = DeriveSemigroup_Magma          op Double
type instance MonoidS     (op :: BinaryNumeric) Double = DeriveMonoid_Semigroup_Neutral op Double

type instance CancellativeS Add Double = CancellativeNum
type instance CancellativeS Mul Double = CancellativeFractional

type instance GroupS        (op :: BinaryNumeric) Double = DeriveGroup_Monoid_Cancellative      op Double
type instance AbelianGroupS (op :: BinaryNumeric) Double = DeriveAbelianGroup_Commutative_Group op Double

-- Rational

type instance MagmaS       (_ :: BinaryNumeric) Rational = MagmaNum
type instance NeutralS     (_ :: BinaryNumeric) Rational = NeutralNum
type instance CommutativeS (_ :: BinaryNumeric) Rational = CommutativeNum

type instance SemigroupS  (op :: BinaryNumeric) Rational = DeriveSemigroup_Magma          op Rational
type instance MonoidS     (op :: BinaryNumeric) Rational = DeriveMonoid_Semigroup_Neutral op Rational

type instance CancellativeS Add Rational = CancellativeNum
type instance CancellativeS Mul Rational = CancellativeFractional

type instance GroupS        (op :: BinaryNumeric) Rational = DeriveGroup_Monoid_Cancellative      op Rational
type instance AbelianGroupS (op :: BinaryNumeric) Rational = DeriveAbelianGroup_Commutative_Group op Rational

type instance MagmaS       (_ :: BinaryNumeric) Rational = MagmaNum
type instance NeutralS     (_ :: BinaryNumeric) Rational = NeutralNum
type instance CommutativeS (_ :: BinaryNumeric) Rational = CommutativeNum

type instance SemigroupS  (op :: BinaryNumeric) Rational = DeriveSemigroup_Magma          op Rational
type instance MonoidS     (op :: BinaryNumeric) Rational = DeriveMonoid_Semigroup_Neutral op Rational

-- Ratio Int8

type instance MagmaS       (_ :: BinaryNumeric) (Ratio Int8) = MagmaNum
type instance NeutralS     (_ :: BinaryNumeric) (Ratio Int8) = NeutralNum
type instance CommutativeS (_ :: BinaryNumeric) (Ratio Int8) = CommutativeNum

type instance SemigroupS  (op :: BinaryNumeric) (Ratio Int8) = DeriveSemigroup_Magma          op (Ratio Int8)
type instance MonoidS     (op :: BinaryNumeric) (Ratio Int8) = DeriveMonoid_Semigroup_Neutral op (Ratio Int8)

type instance CancellativeS Add (Ratio Int8) = CancellativeNum
type instance CancellativeS Mul (Ratio Int8) = CancellativeFractional

type instance GroupS        (op :: BinaryNumeric) (Ratio Int8) = DeriveGroup_Monoid_Cancellative      op (Ratio Int8)
type instance AbelianGroupS (op :: BinaryNumeric) (Ratio Int8) = DeriveAbelianGroup_Commutative_Group op (Ratio Int8)

type instance MagmaS       (_ :: BinaryNumeric) (Ratio Int8) = MagmaNum
type instance NeutralS     (_ :: BinaryNumeric) (Ratio Int8) = NeutralNum
type instance CommutativeS (_ :: BinaryNumeric) (Ratio Int8) = CommutativeNum

type instance SemigroupS  (op :: BinaryNumeric) (Ratio Int8) = DeriveSemigroup_Magma          op (Ratio Int8)
type instance MonoidS     (op :: BinaryNumeric) (Ratio Int8) = DeriveMonoid_Semigroup_Neutral op (Ratio Int8)

-- Ratio Int8

type instance MagmaS       (_ :: BinaryNumeric) (Ratio Int8) = MagmaNum
type instance NeutralS     (_ :: BinaryNumeric) (Ratio Int8) = NeutralNum
type instance CommutativeS (_ :: BinaryNumeric) (Ratio Int8) = CommutativeNum

type instance SemigroupS  (op :: BinaryNumeric) (Ratio Int8) = DeriveSemigroup_Magma          op (Ratio Int8)
type instance MonoidS     (op :: BinaryNumeric) (Ratio Int8) = DeriveMonoid_Semigroup_Neutral op (Ratio Int8)

type instance CancellativeS Add (Ratio Int8) = CancellativeNum
type instance CancellativeS Mul (Ratio Int8) = CancellativeFractional

type instance GroupS        (op :: BinaryNumeric) (Ratio Int8) = DeriveGroup_Monoid_Cancellative      op (Ratio Int8)
type instance AbelianGroupS (op :: BinaryNumeric) (Ratio Int8) = DeriveAbelianGroup_Commutative_Group op (Ratio Int8)

type instance MagmaS       (_ :: BinaryNumeric) (Ratio Int8) = MagmaNum
type instance NeutralS     (_ :: BinaryNumeric) (Ratio Int8) = NeutralNum
type instance CommutativeS (_ :: BinaryNumeric) (Ratio Int8) = CommutativeNum

type instance SemigroupS  (op :: BinaryNumeric) (Ratio Int8) = DeriveSemigroup_Magma          op (Ratio Int8)
type instance MonoidS     (op :: BinaryNumeric) (Ratio Int8) = DeriveMonoid_Semigroup_Neutral op (Ratio Int8)

-- Ratio Int16

type instance MagmaS       (_ :: BinaryNumeric) (Ratio Int16) = MagmaNum
type instance NeutralS     (_ :: BinaryNumeric) (Ratio Int16) = NeutralNum
type instance CommutativeS (_ :: BinaryNumeric) (Ratio Int16) = CommutativeNum

type instance SemigroupS  (op :: BinaryNumeric) (Ratio Int16) = DeriveSemigroup_Magma          op (Ratio Int16)
type instance MonoidS     (op :: BinaryNumeric) (Ratio Int16) = DeriveMonoid_Semigroup_Neutral op (Ratio Int16)

type instance CancellativeS Add (Ratio Int16) = CancellativeNum
type instance CancellativeS Mul (Ratio Int16) = CancellativeFractional

type instance GroupS        (op :: BinaryNumeric) (Ratio Int16) = DeriveGroup_Monoid_Cancellative      op (Ratio Int16)
type instance AbelianGroupS (op :: BinaryNumeric) (Ratio Int16) = DeriveAbelianGroup_Commutative_Group op (Ratio Int16)

type instance MagmaS       (_ :: BinaryNumeric) (Ratio Int16) = MagmaNum
type instance NeutralS     (_ :: BinaryNumeric) (Ratio Int16) = NeutralNum
type instance CommutativeS (_ :: BinaryNumeric) (Ratio Int16) = CommutativeNum

type instance SemigroupS  (op :: BinaryNumeric) (Ratio Int16) = DeriveSemigroup_Magma          op (Ratio Int16)
type instance MonoidS     (op :: BinaryNumeric) (Ratio Int16) = DeriveMonoid_Semigroup_Neutral op (Ratio Int16)

-- Ratio Int32

type instance MagmaS       (_ :: BinaryNumeric) (Ratio Int32) = MagmaNum
type instance NeutralS     (_ :: BinaryNumeric) (Ratio Int32) = NeutralNum
type instance CommutativeS (_ :: BinaryNumeric) (Ratio Int32) = CommutativeNum

type instance SemigroupS  (op :: BinaryNumeric) (Ratio Int32) = DeriveSemigroup_Magma          op (Ratio Int32)
type instance MonoidS     (op :: BinaryNumeric) (Ratio Int32) = DeriveMonoid_Semigroup_Neutral op (Ratio Int32)

type instance CancellativeS Add (Ratio Int32) = CancellativeNum
type instance CancellativeS Mul (Ratio Int32) = CancellativeFractional

type instance GroupS        (op :: BinaryNumeric) (Ratio Int32) = DeriveGroup_Monoid_Cancellative      op (Ratio Int32)
type instance AbelianGroupS (op :: BinaryNumeric) (Ratio Int32) = DeriveAbelianGroup_Commutative_Group op (Ratio Int32)

type instance MagmaS       (_ :: BinaryNumeric) (Ratio Int32) = MagmaNum
type instance NeutralS     (_ :: BinaryNumeric) (Ratio Int32) = NeutralNum
type instance CommutativeS (_ :: BinaryNumeric) (Ratio Int32) = CommutativeNum

type instance SemigroupS  (op :: BinaryNumeric) (Ratio Int32) = DeriveSemigroup_Magma          op (Ratio Int32)
type instance MonoidS     (op :: BinaryNumeric) (Ratio Int32) = DeriveMonoid_Semigroup_Neutral op (Ratio Int32)


-- Ratio Int64

type instance MagmaS       (_ :: BinaryNumeric) (Ratio Int64) = MagmaNum
type instance NeutralS     (_ :: BinaryNumeric) (Ratio Int64) = NeutralNum
type instance CommutativeS (_ :: BinaryNumeric) (Ratio Int64) = CommutativeNum

type instance SemigroupS  (op :: BinaryNumeric) (Ratio Int64) = DeriveSemigroup_Magma          op (Ratio Int64)
type instance MonoidS     (op :: BinaryNumeric) (Ratio Int64) = DeriveMonoid_Semigroup_Neutral op (Ratio Int64)

type instance CancellativeS Add (Ratio Int64) = CancellativeNum
type instance CancellativeS Mul (Ratio Int64) = CancellativeFractional

type instance GroupS        (op :: BinaryNumeric) (Ratio Int64) = DeriveGroup_Monoid_Cancellative      op (Ratio Int64)
type instance AbelianGroupS (op :: BinaryNumeric) (Ratio Int64) = DeriveAbelianGroup_Commutative_Group op (Ratio Int64)

type instance MagmaS       (_ :: BinaryNumeric) (Ratio Int64) = MagmaNum
type instance NeutralS     (_ :: BinaryNumeric) (Ratio Int64) = NeutralNum
type instance CommutativeS (_ :: BinaryNumeric) (Ratio Int64) = CommutativeNum

type instance SemigroupS  (op :: BinaryNumeric) (Ratio Int64) = DeriveSemigroup_Magma          op (Ratio Int64)
type instance MonoidS     (op :: BinaryNumeric) (Ratio Int64) = DeriveMonoid_Semigroup_Neutral op (Ratio Int64)


type instance CancellativeS Add Rational = CancellativeNum
type instance CancellativeS Mul Rational = CancellativeFractional

type instance GroupS        (op :: BinaryNumeric) Rational = DeriveGroup_Monoid_Cancellative      op Rational
type instance AbelianGroupS (op :: BinaryNumeric) Rational = DeriveAbelianGroup_Commutative_Group op Rational

data ComplexLift

type instance MagmaS        (op :: BinaryNumeric) (Complex a) = MagmaNum
type instance NeutralS      (op :: BinaryNumeric) (Complex a) = NeutralNum
type instance CommutativeS  (op :: BinaryNumeric) (Complex a) = CommutativeNum
type instance CancellativeS Add (Complex a) = CancellativeNum
type instance CancellativeS Mul (Complex a) = CancellativeFractional

type instance SemigroupS  (op :: BinaryNumeric) (Complex a) = DeriveSemigroup_Magma          op (Complex a)
type instance MonoidS     (op :: BinaryNumeric) (Complex a) = DeriveMonoid_Semigroup_Neutral op (Complex a)

type instance GroupS        (op :: BinaryNumeric) (Complex a) = DeriveGroup_Monoid_Cancellative      op (Complex a)
type instance AbelianGroupS (op :: BinaryNumeric) (Complex a) = DeriveAbelianGroup_Commutative_Group op (Complex a)
