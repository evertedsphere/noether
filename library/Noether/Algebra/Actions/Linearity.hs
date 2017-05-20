module Noether.Algebra.Actions.Linearity where

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Single
import           Noether.Algebra.Tags

import           Noether.Algebra.Actions.Acts
import           Noether.Algebra.Actions.Compatible

data ActorLinearE = ActorLinear_Acts_Semigroup_Semigroup ActsE SemigroupE SemigroupE
data ActeeLinearE = ActeeLinear_Acts_Semigroup ActsE SemigroupE

type family ActorLinearS (lr :: Side) (act :: k0) (ao :: k1) a (bo :: k2) b :: ActorLinearE
type family ActeeLinearS (lr :: Side) (act :: k0) a (bo :: k2) b :: ActeeLinearE

class ActorLinearK lr act ao a bo b (s :: ActorLinearE)
class ActeeLinearK lr act a bo b (s :: ActeeLinearE)

instance (ActsK lr act a b za, SemigroupK ao a zas, SemigroupK bo b zbs) =>
         ActorLinearK lr act ao a bo b (ActorLinear_Acts_Semigroup_Semigroup za zas zbs)

instance (ActsK lr act a b za, SemigroupK bo b zbs) =>
         ActeeLinearK lr act a bo b (ActeeLinear_Acts_Semigroup za zbs)

-- | (a1 `ao` a2) `act` b = (a1 `act` b) `bo` (a2 `act` b)
type ActorLinearC lr act ao a bo b =
  ActorLinearK lr act ao a bo b (ActorLinearS lr act ao a bo b)

-- | a `act` (b1 `bo` b2) = (a `act` b1) `bo` (a `act` b2)
type ActeeLinearC lr act a bo b =
  ActeeLinearK lr act a bo b (ActeeLinearS lr act a bo b)

type LinearActsOn lr act ao a bo b
  = ActorLinearC lr act ao a bo b
  & ActeeLinearC lr act a bo b
  & Acts lr act a b
  & Semigroup ao a
  & Semigroup bo b

type LinearActs act ao a bo b = LinearActsOn L act ao a bo b & LinearActsOn R act ao a bo b

type LeftLinear act ao a bo b = LinearActsOn L act ao a bo b
type RightLinear act ao a bo b = LinearActsOn R act ao a bo b
