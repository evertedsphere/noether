module Noether.Algebra.Single.Strategies where

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Tags

import           Noether.Algebra.Single.Cancellative
import           Noether.Algebra.Single.Commutative
import           Noether.Algebra.Single.Group
import           Noether.Algebra.Single.Magma
import           Noether.Algebra.Single.Monoid
import           Noether.Algebra.Single.Neutral
import           Noether.Algebra.Single.Semigroup

type instance MagmaS (op :: BinaryBoolean) Bool = MagmaPrim
type instance NeutralS (op :: BinaryBoolean) Bool = NeutralPrim
type instance SemigroupS (op :: BinaryBoolean) Bool =
     FromMagma (MagmaS op Bool)

type instance MagmaS (op :: BinaryNumeric) Double = MagmaNum
type instance NeutralS (op :: BinaryNumeric) Double = NeutralNum
type instance SemigroupS (op :: BinaryNumeric) Double =
     FromMagma (MagmaS op Double)

-- TODO combinator SemigroupNeutralS
type instance MonoidS (op :: BinaryNumeric) Double =
     SemigroupNeutral (SemigroupS op Double) (NeutralS op Double)

type instance CancellativeS Add Double = CancellativeNum
type instance CancellativeS Mul Double = CancellativeFractional

type instance GroupS (op :: BinaryNumeric) Double =
     MonoidCancellative (MonoidS op Double) (CancellativeS op Double)
