module Noether.Algebra.Linear.Strategies where

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Actions
import           Noether.Algebra.Multiple
import           Noether.Algebra.Single
import           Noether.Algebra.Tags

import           Noether.Algebra.Linear.Modules

type DeriveLeftModule_Ring_AbelianGroup p m r a v =
  LeftModule_Ring_AbelianGroup_Linear_Compatible
    (RingS p m r)
    (AbelianGroupS a v)
    (ActorLinearS L m p r a v)
    (ActeeLinearS L m r a v)
    (CompatibleS L m m r v)

type DeriveLeftModule_Self p m r = DeriveLeftModule_Ring_AbelianGroup p m r p r

type DeriveRightModule_Self p m r =
  RightModule_Ring_AbelianGroup_Linear_Compatible
    (RingS p m r)
    (AbelianGroupS p r)
    (ActorLinearS R m p r p r)
    (ActeeLinearS R m r p r)
    (CompatibleS R m m r r)

type instance LeftModuleS Add Mul Double Add Double = DeriveLeftModule_Self Add Mul Double
type instance RightModuleS Add Mul Double Add Double = DeriveRightModule_Self Add Mul Double

a :: LeftModule Add Mul a Add a => a
a = a
