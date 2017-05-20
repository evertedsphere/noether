module Noether.Algebra.Multiple.Ring where

import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Multiple.Semiring
import           Noether.Algebra.Single

type family RingS (add :: ka) (mul :: km) (a :: Type) = (r :: RingE)

data RingE
  = Ring_Semiring_Cancellative { ring_semiring              :: SemiringE
                              ,  ring_addition_cancellative :: CancellativeE}
  | Ring_AbelianGroup_Group { ring_additive_group       :: AbelianGroupE
                           ,  ring_multiplicative_group :: GroupE}
  | RingNamed Symbol
              RingE

class RingK (add :: ka) (mul :: km) a (s :: RingE)

instance (SemiringK p m a zs, CancellativeK p a zpc) =>
         RingK p m a (Ring_Semiring_Cancellative zs zpc)

instance (AbelianGroupK p a zpab, GroupK m a zmg) =>
         RingK p m a (Ring_AbelianGroup_Group zpab zmg)

instance (KnownSymbol sym, RingK p m a s) =>
         RingK p m a (RingNamed sym s)

type RingC p m a = (RingK $$> RingS) p m a

type Ring p m a
  = RingC p m a
  & Semiring p m a
  & AbelianGroup p a
