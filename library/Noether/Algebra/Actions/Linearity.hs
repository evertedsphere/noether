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

-- | (a1 `ao` a2) `act` b = (a1 `act` b) `bo` (a2 `act` b)
type ActorLinearC lr act ao a bo b =
  ActorLinearK lr act ao a bo b (ActorLinearS lr act ao a bo b)

-- | a `act` (b1 `bo` b2) = (a `act` b1) `bo` (a `act` b2)
type ActeeLinearC lr act a bo b =
  ActeeLinearK lr act a bo b (ActeeLinearS lr act a bo b)

type LinearActs lr act ao a bo b
  = ActorLinearC lr act ao a bo b
  & ActeeLinearC lr act a bo b
  & Acts lr act a b
  & Semigroup ao a
  & Semigroup bo b

type LeftLinear act ao a bo b = LinearActs L act ao a bo b
type RightLinear act ao a bo b = LinearActs R act ao a bo b
