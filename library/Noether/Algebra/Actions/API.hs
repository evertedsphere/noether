{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}
module Noether.Algebra.Actions.API
  ( leftAct
  , rightAct
  , leftActK
  , rightActK
  ) where

import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Tags

import           Noether.Algebra.Actions.Acts
import           Noether.Algebra.Actions.Compatible
import           Noether.Algebra.Actions.Linearity

leftActK
  :: forall op s a b. ActsK 'L op a b s => a -> b -> b
leftActK = actK (Proxy :: Proxy op) (Proxy :: Proxy s) (Proxy :: Proxy 'L)

rightActK
  :: forall op s a b. ActsK 'R op a b s => a -> b -> b
rightActK = actK (Proxy :: Proxy op) (Proxy :: Proxy s) (Proxy :: Proxy 'R)

leftAct :: forall op a b. LeftActs op a b => a -> b -> b
leftAct = leftActK @op @(ActsS 'L op a b)

rightAct :: forall op a b. RightActs op a b => a -> b -> b
rightAct = rightActK @op @(ActsS 'R op a b)
