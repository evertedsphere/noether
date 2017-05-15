{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Noether.Algebra.Actions where

import           Data.Complex

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Single.Types

-- | Oh, Either...
data Side = L | R

class ActsK (lr :: k) a b (s :: k') where
  actK :: Proxy s -> Proxy lr -> a -> b -> b

type family ActsS (lr :: k) (a :: Type) (b :: Type) = (r :: k')

type Acts lr a b = ActsK lr a b (ActsS lr a b)

type LeftActs  a b = Acts L a b
type RightActs a b = Acts R a b

-- | A compatible action satisfies
-- a `act` (a' `act` b) = (a `op` a') `act` b
class (Acts lr a b, Semigroup op a) => CompatibleK (lr :: k1) (op :: k2) a b (s :: k3)

type family CompatibleS (lr :: k1) (op :: k2) (a :: Type) (b :: Type) = (r :: k3)

type Compatible lr op a b = CompatibleK lr op a b (CompatibleS lr op a b)

-- | Group actions

type family GSetS (lr :: k1) (op :: k2) (g :: Type) (b :: Type) = (r :: k3)

class (Group op g, Compatible lr op g b) => GSetK lr op g b s

type GSet lr op g b = GSetK lr op g b (GSetS lr op g b)

type LeftGSet  op g b = GSet L op g b
type RightGSet op g b = GSet R op g b

type family ActorLinearS (lr :: k1) (ao :: k2) (a :: Type) (bo :: k3) (b :: Type) = (r :: k3)

class (Acts lr a b, Semigroup ao a, Semigroup bo b) =>
      ActorLinearK lr ao a bo b s

type ActorLinear lr ao a bo b = ActorLinearK lr ao a bo b (ActorLinearS lr ao a bo b)

type family ActeeLinearS (lr :: k1) (ao :: k2) (a :: Type) (bo :: k3) (b :: Type) = (r :: k3)

class (Acts lr a b, Semigroup ao a, Semigroup bo b) =>
      ActeeLinearK lr ao a bo b s

type ActeeLinear lr ao a bo b = ActeeLinearK lr ao a bo b (ActeeLinearS lr ao a bo b)

type LinearActs lr ao a bo b = (ActorLinear lr ao a bo b, ActeeLinear lr ao a bo b)

type LeftLinear  ao a bo b = LinearActs L ao a bo b
type RightLinear ao a bo b = LinearActs R ao a bo b

type LeftCompatible  ao a b = Compatible L ao a b
type RightCompatible ao a b = Compatible R ao a b

data Known


