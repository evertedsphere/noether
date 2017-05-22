{-# LANGUAGE CPP #-}
module Noether.Algebra.Actions.Strategies where

import           Noether.Algebra.Single
import           Noether.Algebra.Tags
import           Noether.Lemmata.Prelude

import           Noether.Algebra.Actions.Acts
import           Noether.Algebra.Actions.Compatible
import           Noether.Algebra.Actions.Linearity

import           Noether.Algebra.Actions.API

type DeriveActs_Tagged tag lr op a b =
  ActsTagged tag (ActsS lr op a b)

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

-- Forgive me.

#define self_action(ty) \
type instance ActsS lr (op :: BinaryNumeric) (ty) (ty) = DeriveActs_Magma op (ty); \
type instance CompatibleS lr (op :: BinaryNumeric) op (ty) (ty) = DeriveCompatible_Associativity lr op (ty); \
type instance ActorLinearS lr Mul Add (ty) Add (ty) = DeriveActorLinearActs_LeftDistributivity lr Add Mul (ty); \
type instance ActeeLinearS lr Mul (ty) Add (ty) = DeriveActeeLinearActs_RightDistributivity lr Add Mul (ty)

self_action(Double)
self_action(Rational)
self_action(Complex Double)
