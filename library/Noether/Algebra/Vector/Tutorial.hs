{-| A work-in-progress tutorial for Noether vectors.

    This module demonstrates creating vectors, using simple operators to
    transform them, and techniques that leverage type system features to provide
    improved correctness guarantees.
-}
module Noether.Algebra.Vector.Tutorial
  (
  -- * Basics
  -- $basics

  -- * Computing with statically differentiable but unknown dimensions, a la subhask
  -- $action

  -- $sizedvectors
  ) where

import           Noether.Algebra.Tags
import           Noether.Lemmata.Prelude

import           Noether.Algebra.Actions
import           Noether.Algebra.Linear
import           Noether.Algebra.Single

import           Noether.Algebra.Vector.Boxed

{- $basics

  Vectors are constructed with 'unsafeFromList', which wraps
  'Data.Vector.Generic.fromList' under the hood.

> v :: BVector 10 (Complex Double)
> v = unsafeFromList $ map (\x -> cis (x * pi / 10)) [1..10]

> w :: BVector 10 (Complex Double)
> w = unsafeFromList $ map (\x -> cis (-x * pi / 10)) [1..10]

  The type annotations actually do nothing but bring the KnownNat constraint
  into scope. (TODO demo with PartialTypeSignatures?)

> func x = x + x - x * x + zero

  The inferred type of @func@ is somewhat hairy:

> func
>   :: ( CancellativeK 'Add a (CancellativeS 'Add a)
>      , MagmaK 'Add a (MagmaS 'Add a)
>      , MagmaK 'Mul a (MagmaS 'Mul a)
>      , NeutralK 'Add a (NeutralS 'Add a)
>      ) => a -> a

  which is an expanded version of

> func
>   :: ( Cancellative 'Add a
>      , Magma 'Add a
>      , Magma 'Mul a
>      , Neutral 'Add a
>      ) => a -> a

  but can be understood as asking for a type @a@ that supports subtraction
  ('Cancellative' 'Add'), addition ('Magma' 'Add'), and multiplication ('Magma'
  'Mul'), and has a zero ('Neutral' 'Add').

> g = map (\lambda -> lerp (lambda :+ 0) v (func w)) [0.0,0.1..1.0 :: Double]

-}

{- $action

   Given an action of @a@ on @b@, @a %< b@ computes the result. Usually, this is
   just multiplication. Other interesting examples exist: e.g. group actions,
   where @b@ is just a set, and so on.

   A particularly pervasive one is the action of Z on groups:

prop> n %< a = a + a + ... + a

   where the right side is a added to itself n times.
-}

{-

> g :: [BVector 10 (Complex Double)]
-}

{- $sizedvectors

> u1 :: BVector "a" Double
> u1 = unsafeFromList [1..10]

> u2 :: BVector "b" Double
> u2 = unsafeFromList [1..10]

> u3 :: BVector "a" Double
> u3 = unsafeFromList [1..10]

> s = u1 + x %< u3
>   where
>     x = 0.3 :: Double

  This fails:

> t = u1 + u2

>   • Couldn't match type ‘"b"’ with ‘"a"’
>     Expected type: BVector "a" Double
>       Actual type: BVector "b" Double

  You need to do the whole "I know what I'm doing":

> t = u1 + unsafeChangeDimension u3

-}
