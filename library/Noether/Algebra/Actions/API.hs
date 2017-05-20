{-# LANGUAGE AllowAmbiguousTypes #-}
module Noether.Algebra.Actions.API
  ( leftAct
  , rightAct
  ) where

import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Tags

import           Noether.Algebra.Actions.Acts
import           Noether.Algebra.Actions.Compatible
import           Noether.Algebra.Actions.Linearity

leftActK
  :: forall a b s op. ActsK 'L op a b s
  => Proxy op -> Proxy s -> a -> b -> b
leftActK opP sP = actK opP sP (Proxy :: Proxy 'L)

rightActK
  :: forall a b s op. ActsK 'R op a b s
  => Proxy op -> Proxy s -> a -> b -> b
rightActK opP sP = actK opP sP (Proxy :: Proxy 'R)

leftAct :: forall a b op. LeftActs op a b => a -> b -> b
leftAct = leftActK (Proxy :: Proxy op) (Proxy :: Proxy (ActsS 'L op a b))

rightAct :: forall a b op. RightActs op a b => a -> b -> b
rightAct = rightActK (Proxy :: Proxy op) (Proxy :: Proxy (ActsS 'R op a b))

-- a :: RightActs Double a => a -> a
-- a = rightAct (3 :: Double)
