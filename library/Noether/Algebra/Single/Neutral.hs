{-# LANGUAGE TypeApplications #-}
module Noether.Algebra.Single.Neutral where

import qualified Prelude                 as P

import           Noether.Lemmata.Prelude hiding (Num)
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Tags

data NeutralE
  = NeutralPrim
  | NeutralNum
  | NeutralNamed Symbol NeutralE
  | NeutralTagged Type NeutralE

class NeutralK (op :: k) a (s :: NeutralE) where
  neutralK :: Proxy op -> Proxy s -> a

instance P.Num a => NeutralK Add a NeutralNum where
  neutralK _ _ = 0

instance P.Num a => NeutralK Mul a NeutralNum where
  neutralK _ _ = 1

instance NeutralK And P.Bool NeutralPrim where
  neutralK _ _ = True

instance NeutralK Or P.Bool NeutralPrim where
  neutralK _ _ = False

instance (KnownSymbol sym, NeutralK op a s) => NeutralK op a (NeutralNamed sym s) where
  neutralK opP _ = neutralK opP (Proxy @s)

type family NeutralS (op :: k) (a :: Type) = (r :: NeutralE)
type Neutral op a = NeutralK op a (NeutralS op a)
