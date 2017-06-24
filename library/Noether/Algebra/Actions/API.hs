{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}
module Noether.Algebra.Actions.API where

import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Tags

import           Noether.Algebra.Actions.Acts
import           Noether.Algebra.Actions.Compatible
import           Noether.Algebra.Actions.Linearity

import           Noether.Algebra.Single

type CompatibleC lr op act a b = CompatibleK lr op act a b (CompatibleS lr op act a b)

type Compatible lr op act a b
  = ( CompatibleC lr op act a b
    , Semigroup op a
    , Acts lr act a b)

type LeftCompatible act ao a b = Compatible L act ao a b
type RightCompatible act ao a b = Compatible R act ao a b

type Acts lr op a b = ActsK lr op a b (ActsS lr op a b)

type LeftActs  op a b = Acts L op a b
type RightActs op a b = Acts R op a b

type BiActs op a b = (LeftActs op a b, RightActs op a b)


leftActK
  :: forall op s a b. ActsK 'L op a b s => a -> b -> b
leftActK = actK (Proxy @op) (Proxy @s) (Proxy @'L)

rightActK
  :: forall op s a b. ActsK 'R op a b s => a -> b -> b
rightActK = actK (Proxy @op) (Proxy @s) (Proxy @'R)

leftAct :: forall op a b. LeftActs op a b => a -> b -> b
leftAct = leftActK @op @(ActsS 'L op a b)

rightAct :: forall op a b. RightActs op a b => a -> b -> b
rightAct = rightActK @op @(ActsS 'R op a b)

-- | > (a1 `ao` a2) `act` b = (a1 `act` b) `bo` (a2 `act` b)
type ActorLinearC lr act ao a bo b =
  ActorLinearK lr act ao a bo b (ActorLinearS lr act ao a bo b)

-- | > a `act` (b1 `bo` b2) = (a `act` b1) `bo` (a `act` b2)
type ActeeLinearC lr act a bo b =
  ActeeLinearK lr act a bo b (ActeeLinearS lr act a bo b)

type LinearActsOn lr act ao a bo b
  = ( ActorLinearC lr act ao a bo b
    , ActeeLinearC lr act a bo b
    , Acts lr act a b
    , Semigroup ao a
    , Semigroup bo b)

type LinearActs act ao a bo b = (LinearActsOn L act ao a bo b, LinearActsOn R act ao a bo b)

type LeftLinear act ao a bo b = LinearActsOn L act ao a bo b
type RightLinear act ao a bo b = LinearActsOn R act ao a bo b
