{-# LANGUAGE AllowAmbiguousTypes #-}
module Noether.Algebra.Vector.Generic where

import qualified Data.Vector.Generic      as G

import           Noether.Lemmata.TypeFu
import qualified Prelude                  as P

import           Noether.Algebra.Actions
import           Noether.Algebra.Multiple
import           Noether.Algebra.Single

gBinaryOpK
  :: forall v a s op z.
     (MagmaK op a s, G.Vector v a, Coercible v z)
  => Proxy op -> z a -> z a -> z a
gBinaryOpK o x y = coerce (G.zipWith binop (coerce x :: v a) (coerce y :: v a))
    where
      binop = binaryOpK o (Proxy :: Proxy s)

gCancelK
  :: forall v a s op z.
     (CancellativeK op a s, G.Vector v a, Coercible v z)
  => Proxy op -> z a -> z a
gCancelK o x = coerce (G.map cancelK' (coerce x :: v a))
    where
      cancelK' = cancelK o (Proxy :: Proxy s)

gActK
  :: forall v a b s lr op z.
     (ActsK lr op a b s, G.Vector v b, Coercible v z)
  => Proxy op -> a -> z b -> z b
gActK o a x = coerce (G.map (actK' a) (coerce x :: v b))
    where
      actK' = actK o (Proxy :: Proxy s) (Proxy :: Proxy lr)

gNeutralK
  :: forall n v a s op b.
     (NeutralK op a s, KnownNat n, G.Vector v a, Coercible v (b n))
  => Proxy op -> b n a
gNeutralK o = coerce (G.replicate count neutralValue :: v a)
    where
      count = P.fromIntegral (natVal (Proxy :: Proxy n))
      neutralValue = neutralK o (Proxy :: Proxy s)

