{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
module Noether.Algebra.Linear.Strategies where

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Actions
import           Noether.Algebra.Multiple
import           Noether.Algebra.Single
import           Noether.Algebra.Single.Synonyms
import           Noether.Algebra.Tags

import           Noether.Algebra.Linear.Module

type DeriveLeftModule_Ring_AbelianGroup op p m r a v =
  LeftModule_Ring_AbelianGroup_Linear_Compatible
    (RingS p m r)
    (AbelianGroupS a v)
    (ActorLinearS L m p r a v)
    (ActeeLinearS L m r a v)
    (CompatibleS L op m r v)

type DeriveLeftModule_Self p m r = DeriveLeftModule_Ring_AbelianGroup m p m r p r

type DeriveRightModule_Ring_AbelianGroup op p m r a v =
  RightModule_Ring_AbelianGroup_Linear_Compatible
    (RingS p m r)
    (AbelianGroupS a v)
    (ActorLinearS R m p r a v)
    (ActeeLinearS R m r a v)
    (CompatibleS R op m r v)

type DeriveRightModule_Self p m r = DeriveRightModule_Ring_AbelianGroup m p m r p r

type instance LeftModuleS Mul Add Mul Double Add Double =
     DeriveLeftModule_Self Add Mul Double

type instance RightModuleS Mul Add Mul Double Add Double =
     DeriveRightModule_Self Add Mul Double
