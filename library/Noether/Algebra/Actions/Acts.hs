module Noether.Algebra.Actions.Acts where

import           Data.Complex

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Single
import           Noether.Algebra.Tags

data ActsE =
  Acts_Magma MagmaE

class ActsK (lr :: Side) (op :: k) a b (s :: ActsE) where
  actK :: Proxy op -> Proxy s -> Proxy lr -> a -> b -> b

type family ActsS (lr :: Side) (op :: k) (a :: Type) (b :: Type) = (r :: ActsE)

type Acts lr op a b = ActsK lr op a b (ActsS lr op a b)

type LeftActs  op a b = Acts L op a b
type RightActs op a b = Acts R op a b

instance MagmaK op a zm => ActsK lr op a a (Acts_Magma zm) where
  actK opP _ _ = binaryOpK opP (Proxy :: Proxy zm)

