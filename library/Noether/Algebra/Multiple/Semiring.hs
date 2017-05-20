module Noether.Algebra.Multiple.Semiring where

import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Single

type family SemiringS (add :: ka) (mul :: km) (a :: Type) = (r :: SemiringE)

data SemiringE
  = Semiring_Commutative_Monoid_Monoid { semiring_commutative :: CommutativeE
                                       , semiring_additive_monoid :: MonoidE
                                       , semiring_multiplicative_monoid :: MonoidE}
  | SemiringNamed Symbol
                  SemiringE

class SemiringK (add :: ka) (mul :: km) a (s :: SemiringE)

instance (MonoidK p a zam, CommutativeK p a zac, MonoidK m a zbm) =>
         SemiringK p m a (Semiring_Commutative_Monoid_Monoid zac zam zbm)

instance (KnownSymbol sym, SemiringK p m a s) =>
         SemiringK p m a (SemiringNamed sym s)

type SemiringC p m a = (SemiringK $$> SemiringS) p m a

type Semiring p m a
  = SemiringC p m a
  & Commutative p a
  & Monoid p a
  & Monoid m a
