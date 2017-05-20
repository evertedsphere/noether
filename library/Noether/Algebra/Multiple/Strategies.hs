{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
module Noether.Algebra.Multiple.Strategies where

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Tags

import           Noether.Algebra.Multiple.Ring
import           Noether.Algebra.Multiple.Semiring
import           Noether.Algebra.Single

type Ring p m a =
  ( RingC p m a
  , Group m a
  , AbelianGroup p a
  )

type Semiring p m a =
  ( SemiringC p m a
  , Commutative p a
  , Monoid p a
  , Monoid m a
  )

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

type family RingNamedT a where
  RingNamedT (Ring_AbelianGroup_Group ab grp) =
    Ring_AbelianGroup_Group
      (AbelianGroupNamed "Additive group" ab)
      (GroupNamed "Multiplicative group" grp)

type DeriveRingDoc_AbelianGroup_Group p m a =
  RingNamedT (DeriveRing_AbelianGroup_Group p m a)

type instance SemiringS Add Mul Double =
     DeriveSemiring_Commutative_Monoid_Monoid Add Mul Double

type instance RingS Add Mul Double =
     DeriveRingDoc_AbelianGroup_Group Add Mul Double

p :: Ring Add Mul a => a -> a -> a
p a b = a + b / (b - a) * b + (b + a / b * b - a)

q :: Double
q = p 2 (p 3 4)
