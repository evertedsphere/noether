module Noether.Algebra.Multiple.Strategies where

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Tags

import           Noether.Algebra.Multiple.Ring
import           Noether.Algebra.Multiple.Semiring
import           Noether.Algebra.Single

-- Lifting strategies

type DeriveSemiring_Commutative_Monoid_Monoid p m a =
  Semiring_Commutative_Monoid_Monoid
    (CommutativeS p a)
    (MonoidS p a)
    (MonoidS m a)

type DeriveRing_Semiring_Cancellative p m a =
  Ring_Semiring_Cancellative
    (SemiringS p m a)
    (CancellativeS p a)

type DeriveRing_AbelianGroup_Group p m a =
  Ring_AbelianGroup_Group
    (AbelianGroupS p a)
    (GroupS m a)

type DeriveRingDoc_AbelianGroup_Group p m a =
  Ring_AbelianGroup_Group
    (AbelianGroupNamed "Additive group" (AbelianGroupS p a))
    (GroupNamed "Multiplicative group" (GroupS m a))

type instance SemiringS Add Mul Double =
     DeriveSemiring_Commutative_Monoid_Monoid Add Mul Double

type instance RingS Add Mul Double =
     DeriveRing_AbelianGroup_Group Add Mul Double
