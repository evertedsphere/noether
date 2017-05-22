module Noether.Algebra.Vector.Tutorial where

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

-- | This is equal to
-- > BVector [5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0]

-- g :: [BVector 10 (Complex Double)]
g = map (\γ -> lerp (γ :+ 0) v (f w)) [0.0,0.1..1.0 :: Double]

{-
   Computing with statically differentiable but unknown dimensions, a la subhask
-}

u1 :: BVector "a" Double
u1 = unsafeFromList [1..10]

u2 :: BVector "b" Double
u2 = unsafeFromList [1..10]

u3 :: BVector "a" Double
u3 = unsafeFromList [1..10]

s = u1 + x %< u3
  where x = 0.3 :: Double

-- • Couldn't match type ‘"b"’ with ‘"a"’
--   Expected type: BVector "a" Double
--     Actual type: BVector "b" Double
-- t = u1 + u2
-- You need to do the whole "I know what I'm doing":

t = u1 + unsafeChangeDimension u3
