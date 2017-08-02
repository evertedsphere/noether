{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Noether.Algebra.Linear.Module where

import           Data.Complex

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Actions
import           Noether.Algebra.Multiple
import           Noether.Algebra.Single

import           Noether.Algebra.Tags

data LeftModuleE
  = LeftModule_Ring_AbelianGroup_Linear_Compatible
    { leftModule_ring           :: RingE
    , leftModule_abelianGroup   :: AbelianGroupE
    , leftModule_actorLinearity :: ActorLinearE
    , leftModule_acteeLinearity :: ActeeLinearE
    , leftModule_compatibility  :: CompatibleE}
  | LeftModule_Named Symbol
                     LeftModuleE

-- | A left module (v, a) over the ring (r, p, m).
class LeftModuleK op p m r a v s

type family LeftModuleS (op :: k0) (p :: k1) (m :: k2) r (a :: k3) v :: LeftModuleE

type LeftModuleC op p m r a v = LeftModuleK op p m r a v (LeftModuleS op p m r a v)

instance ( RingK p m r zr
         , AbelianGroupK a v zag
         , ActorLinearK L m p r a v zor
         , ActeeLinearK L m r a v zee
         , CompatibleK L op m r v zlc
         ) =>
         LeftModuleK op p m r a v
           (LeftModule_Ring_AbelianGroup_Linear_Compatible zr zag zor zee zlc)

type LeftModule op p m r a v =
  ( LeftModuleC op p m r a v
  , Ring p m r
  , AbelianGroup a v
  , LinearActsOn L m p r a v
  , LeftCompatible op m r v
  )

data RightModuleE
  = RightModule_Ring_AbelianGroup_Linear_Compatible
    { rightModule_ring           :: RingE
    , rightModule_abelianGroup   :: AbelianGroupE
    , rightModule_actorLinearity :: ActorLinearE
    , rightModule_acteeLinearity :: ActeeLinearE
    , rightModule_compatibility  :: CompatibleE}
  | RightModule_Named Symbol
                     RightModuleE

-- | A right module (v, a) over the ring (r, p, m).
class RightModuleK op p m r a v s

type family RightModuleS (op :: k0) (p :: k1) (m :: k2) r (a :: k3) v :: RightModuleE

type RightModuleC op p m r a v = RightModuleK op p m r a v (RightModuleS op p m r a v)

instance ( RingK p m r zr
         , AbelianGroupK a v zag
         , ActorLinearK 'R m p r a v zor
         , ActeeLinearK 'R m r a v zee
         , CompatibleK 'R op m r v zrc
         ) => RightModuleK op p m r a v
           (RightModule_Ring_AbelianGroup_Linear_Compatible zr zag zor zee zrc)

type RightModule op p m r a v =
  ( RightModuleC op p m r a v
  , Ring p m r
  , AbelianGroup a v
  , LinearActsOn 'R m p r a v
  , RightCompatible op m r v
  )
