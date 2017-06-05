module Noether.Algebra.Single.Magma where

import qualified Prelude                 as P

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Tags

data MagmaE
  = MagmaPrim
  | MagmaNum
  | MagmaNamed Symbol MagmaE
  | MagmaTagged Type MagmaE

class MagmaK (op :: k) a (s :: MagmaE) where
  binaryOpK :: Proxy op -> Proxy s -> a -> a -> a

instance P.Num a => MagmaK Add a MagmaNum where
  binaryOpK _ _ = (P.+)

instance P.Num a => MagmaK Mul a MagmaNum where
  binaryOpK _ _ = (P.*)

instance MagmaK And P.Bool MagmaPrim where
  binaryOpK _ _ = (P.&&)

instance MagmaK Or P.Bool MagmaPrim where
  binaryOpK _ _ = (P.||)

instance MagmaK op a s => MagmaK op a (MagmaNamed sym s) where
  binaryOpK o _ = binaryOpK o (Proxy :: Proxy (MagmaNamed sym s))

instance MagmaK op a s => MagmaK op (i -> a) (MagmaTagged FunctionLift s) where
  binaryOpK o _ f g = \x -> f x `binop` g x
    where binop = binaryOpK o (Proxy :: Proxy s)

type Magma op a = MagmaK op a (MagmaS op a)

type family MagmaS (op :: k) (a :: Type) = (r :: MagmaE)
