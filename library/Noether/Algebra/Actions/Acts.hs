{-# LANGUAGE TypeApplications #-}
module Noether.Algebra.Actions.Acts where

import           Data.Complex

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Single
import           Noether.Algebra.Tags

data ActsE
  = Acts_Magma MagmaE
  | ActsNamed Symbol ActsE
  | ActsTagged Type ActsE

class ActsK (lr :: Side) (op :: k) a b (s :: ActsE) where
  actK :: Proxy op -> Proxy s -> Proxy lr -> a -> b -> b

type family ActsS (lr :: Side) (op :: k) (a :: Type) (b :: Type) = (r :: ActsE)

instance MagmaK op a zm => ActsK lr op a a (Acts_Magma zm) where
  actK opP _ _ = binaryOpK opP (Proxy @zm)

