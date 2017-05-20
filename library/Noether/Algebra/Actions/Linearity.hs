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

