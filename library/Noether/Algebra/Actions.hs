module Noether.Algebra.Actions
  ( leftAct
  , rightAct
  , module Types
  ) where

import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Actions.Types as Types

leftActK :: forall a b s. ActsK 'L a b s => Proxy s -> a -> b -> b
leftActK p = actK p (Proxy :: Proxy 'L)

rightActK :: forall a b s. ActsK 'R a b s => Proxy s -> a -> b -> b
rightActK p = actK p (Proxy :: Proxy 'R)

leftAct :: forall a b. LeftActs a b => a -> b -> b
leftAct = leftActK (Proxy :: Proxy (ActsS 'L a b))

rightAct :: forall a b. RightActs a b => a -> b -> b
rightAct = rightActK (Proxy :: Proxy (ActsS 'R a b))

-- a :: RightActs Double a => a -> a
-- a = rightAct (3 :: Double)
