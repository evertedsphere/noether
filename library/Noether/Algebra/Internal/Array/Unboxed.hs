module Noether.Algebra.Internal.Array.Unboxed where

import qualified Prelude                 as P

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Single
import           Noether.Algebra.Tags

import           Data.Vector.Unboxed     as U


vec :: Vector Double
vec = U.replicate 20 0

vec2 :: Vector Double
vec2 = U.replicate 20 1

data VectorLift

type instance MagmaS Add (Vector Double) =
     MagmaTagged VectorLift (MagmaS Add Double)

instance (Unbox v, MagmaK op v s) =>
         MagmaK op (Vector v) (MagmaTagged VectorLift s) where
  binaryOpK o _ = U.zipWith (binaryOpK o (Proxy :: Proxy s))

-- newtype Array (n :: Nat) v = Array (U.Vector v)
