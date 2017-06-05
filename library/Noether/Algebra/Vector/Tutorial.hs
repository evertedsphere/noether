module Noether.Algebra.Vector.Tutorial where

import           Noether.Algebra.Tags
import           Noether.Lemmata.Prelude

import           Noether.Algebra.Actions
import           Noether.Algebra.Linear
import           Noether.Algebra.Single

import           Noether.Algebra.Vector.Boxed

{-
   Basic demo
-}

-- | This annotation actually does nothing but bring the KnownNat constraint
-- into scope. (TODO demo with PartialTypeSignatures?)
v :: BVector 10 (Complex Double)
v = unsafeFromList $ map (\x -> cis (x * pi / 10)) [1..10]

-- w :: BVector 10 (Complex Double)
w = unsafeFromList $ map (\x -> cis (-x * pi / 10)) [1..10]

-- f :: (CancellativeK 'Add a (CancellativeS 'Add a),
--       MagmaK 'Add a (MagmaS 'Add a), MagmaK 'Mul a (MagmaS 'Mul a),
--       NeutralK 'Add a (NeutralS 'Add a)) =>
--      a -> a
f x = x + x - x * x + zero

-- g :: [BVector 10 (Complex Double)]
g = map (\lambda -> lerp (lambda :+ 0) v (f w)) [0.0,0.1..1.0 :: Double]

{-
   Computing with statically differentiable but unknown dimensions, a la subhask
-}

u1 :: BVector "a" Double
u1 = unsafeFromList [1..10]

u2 :: BVector "b" Double
u2 = unsafeFromList [1..10]

u3 :: BVector "a" Double
u3 = unsafeFromList [1..10]

-- Given an action of the a on b, a %< b computes the results. Usually,
-- this is just multiplication. Other interesting examples exist: e.g. group
-- actions, where b is just a set, and so on.
--
-- A particularly pervasive one is the action of Z on groups:
-- > n %< a = a + a + ... + a
-- where the right side is a added to itself n times.

s = u1 + x %< u3
  where
    x = 0.3 :: Double

-- This fails:
-- > t = u1 + u2
-- • Couldn't match type ‘"b"’ with ‘"a"’
--   Expected type: BVector "a" Double
--     Actual type: BVector "b" Double

-- You need to do the whole "I know what I'm doing":

t = u1 + unsafeChangeDimension u3

