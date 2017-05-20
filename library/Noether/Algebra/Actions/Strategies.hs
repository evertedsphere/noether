module Noether.Algebra.Actions.Strategies where

import           Noether.Algebra.Single
import           Noether.Algebra.Tags
import           Noether.Lemmata.Prelude

import           Noether.Algebra.Actions.Acts
import           Noether.Algebra.Actions.Compatible
import           Noether.Algebra.Actions.Linearity

import           Noether.Algebra.Actions.API

type DeriveActs_Magma op a =
  Acts_Magma (MagmaS op a)

type DeriveCompatible_Acts_Semigroup lr op act a b =
  Compatible_Acts_Semigroup a
    (ActsS lr act a b)
    (SemigroupS op a)

-- | a + (b + c) = (a + b) + c
type DeriveCompatible_Associativity lr op a =
  DeriveCompatible_Acts_Semigroup lr op op a a

type DeriveActorLinearActs_Acts_Semigroup_Semigroup lr act ao a bo b =
  ActorLinear_Acts_Semigroup_Semigroup
    (ActsS lr act a b)
    (SemigroupS ao a)
    (SemigroupS bo b)

type DeriveActeeLinearActs_Acts_Semigroup lr act a bo b =
  ActeeLinear_Acts_Semigroup
    (ActsS lr act a b)
    (SemigroupS bo b)

-- | (a + b) * c = a * c + b * c
type DeriveActorLinearActs_LeftDistributivity lr p m a =
  DeriveActorLinearActs_Acts_Semigroup_Semigroup lr m p a p a

-- | a * (b + c) = a * b + a * c
type DeriveActeeLinearActs_RightDistributivity lr p m a =
  DeriveActeeLinearActs_Acts_Semigroup lr m a p a

type instance ActsS lr (op :: BinaryNumeric) Double Double =
     DeriveActs_Magma op Double

type instance CompatibleS lr (op :: BinaryNumeric) op Double Double =
     DeriveCompatible_Associativity lr op Double

type instance ActorLinearS lr Mul Add Double Add Double =
     DeriveActorLinearActs_LeftDistributivity lr Add Mul Double

type instance ActeeLinearS lr Mul Double Add Double =
     DeriveActeeLinearActs_RightDistributivity lr Add Mul Double

