{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Noether.Algebra.Actions.Types where

import           Data.Complex

import           Noether.Lemmata.Prelude

import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Inference
import           Noether.Algebra.Single.Types

-- | Oh, Either...
data Side = L | R

class ActsK (lr :: k) a b (s :: k') where
  actK :: Proxy s -> Proxy lr -> a -> b -> b

type family ActsS (lr :: k) (a :: Type) (b :: Type) = (r :: Type)

type Acts lr a b = ActsK lr a b (ActsS lr a b)

type LeftActs  a b = Acts L a b
type RightActs a b = Acts R a b

{-| A strategy-parameterized typeclass for a compatible action, where compatibility
    is defined in the group action sense.

    A compatible action satisfies
    a `act` (a' `act` b) = (a `op` a') `act` b
-}
class CompatibleK (lr :: k1) (op :: k2) tor tee (s :: k3)

type family CompatibleS (lr :: k1) (op :: k2) (a :: Type) (b :: Type) = (r :: Type)

type CompatibleC lr op a b = CompatibleK lr op a b (CompatibleS lr op a b)

type Compatible lr op a b
  = CompatibleC lr op a b
  & Semigroup op a
  & Acts lr a b

--------------------------------------------------------------------------------
-- Group actions
--------------------------------------------------------------------------------

type family GSetS (lr :: k1) (op :: k2) (g :: Type) (b :: Type) = (r :: Type)

class GSetK lr op g b s

type GSetC lr op g b = GSetK lr op g b (GSetS lr op g b)

type GSet lr op g b
  = GSetC lr op g b
  & Compatible lr op g b
  & Group op g

type LeftGSet  op g b = GSet L op g b
type RightGSet op g b = GSet R op g b

--------------------------------------------------------------------------------
-- Linearity
--------------------------------------------------------------------------------

type family ActorLinearS (lr :: k1) (ao :: k2) (a :: Type) (bo :: k3) (b :: Type) = (r :: Type)
type family ActeeLinearS (lr :: k1) (ao :: k2) (a :: Type) (bo :: k3) (b :: Type) = (r :: Type)

class ActorLinearK lr ao a bo b s
class ActeeLinearK lr ao a bo b s

type ActorLinearC lr ao a bo b = ActorLinearK lr ao a bo b (ActorLinearS lr ao a bo b)
type ActeeLinearC lr ao a bo b = ActeeLinearK lr ao a bo b (ActeeLinearS lr ao a bo b)

type LinearActs lr ao a bo b
  = ActorLinearC lr ao a bo b
  & ActeeLinearC lr ao a bo b
  & Acts lr a b
  & Semigroup ao a
  & Semigroup bo b

type LeftLinear  ao a bo b = LinearActs L ao a bo b
type RightLinear ao a bo b = LinearActs R ao a bo b

type LeftCompatible  ao a b = Compatible L ao a b
type RightCompatible ao a b = Compatible R ao a b

--------------------------------------------------------------------------------
-- Derived instances
--------------------------------------------------------------------------------

instance (ActsK lr a b za, SemigroupK op a zs) =>
         CompatibleK lr op a b (Synergise '["Acts" := za, "Semigroup" := zs])

instance (GroupK op g zg, CompatibleK lr op g b zc) =>
         GSetK lr op g b (Synergise '["Compatible" := zc, "Group" := zg])

instance ( ActsK lr a b za
         , SemigroupK ao a zsa
         , SemigroupK bo b zsb
         ) => ActorLinearK lr ao a bo b (Synergise
           '[ "Acts" := za
            , "Semigroup (actor)" := zsa
            , "Semigroup (actee)" := zsb
            ])

instance ( ActsK lr a b za
         , SemigroupK ao a zsa
         , SemigroupK bo b zsb
         ) => ActeeLinearK lr ao a bo b (Synergise
           '[ "Acts" := za
            , "Semigroup (actor)" := zsa
            , "Semigroup (actee)" := zsb
            ])
