{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Algebra.LinearMaps where

import           Algebra.Basics
import           Algebra.Modules
import           Prelude         hiding (Monoid, fromInteger, negate, recip,
                                  (*), (+), (-), (/))

import           Control.Arrow   ((&&&))
import           Data.Complex
import           Data.Proxy
import           Data.Kind       (type (*))

---------------------------------------------------------------------------
-- Linear maps in the style of Conal Elliott,
-- adapted for compatibility with All The Polymorphism™
-- from <http://conal.net/blog/posts/reimagining-matrices>
-- Likely not performant, but definitely pretty and a good place
-- to test the abstractions developed so far.
---------------------------------------------------------------------------

infixr 1 \>
infixr 0 ~>

-- | Postfix "over" operator, akin to ($)
type (~>) a b = a b

-- | The type of linear maps from a to b is written
--     k \> a ~> b
--   where a and b are k-vector spaces. This notation might
--   be read "`a` to `b` over `k`".

-- | DotProductSpace should more appropriately be something like "free module".
-- The reason relaxing the constraints in the (:&&) constructor to `VectorSpace'`
-- doesn't work is because we are doing something coordinate-wise in some sense,
-- I believe.
data (\>) :: (* -> * -> * -> *) where
  Dot
    :: DotProductSpace' k a
    => a
    -> k \> a ~> k
  (:&&)
    ::
     ( VectorSpace' k a
     , DotProductSpace' k b
     , DotProductSpace' k c
     ) => k \> a ~>  b
       -> k \> a ~>     c
       -> k \> a ~> (b, c)

-- A couple exercises of the syntax:

-- | A π/2 counterclockwise rotation in R^2.
rotate90 :: Double \> (Double, Double) ~> (Double, Double)
rotate90 = Dot (0, -1)
       :&& Dot (1,  0)

-- | A 𝜑 counterclockwise rotation in R^2.
rotate :: Double -> Double \> (Double, Double) ~> (Double, Double)
rotate phi = Dot ( cos phi, -sin phi)
         :&& Dot ( sin phi,  cos phi)

------------------------------------------------------------------------------------
-- Primitive operations on linear maps
------------------------------------------------------------------------------------

-- | Converts a linear map into a function.
apply
  :: k \> a ~> b
  -> a -> b
apply (Dot b)   = dot b
apply (f :&& g) = \v -> (apply f v, apply g v)

-- | For the monoid etc. instances
addLinearMap
  :: k \> a ~> b
  -> k \> a ~> b
  -> k \> a ~> b
addLinearMap (Dot x) (Dot y) = Dot (x + y)
addLinearMap (x :&& w) (y :&& z) =
  (x `addLinearMap` y) :&& (w `addLinearMap` z)
addLinearMap _ _ = error "Unexpected args"

-- | For the vector space instances
scaleLMap
  :: k
  -> k \> a ~> b
  -> k \> a ~> b
scaleLMap lambda (Dot x)   = Dot (lambda %< x)
scaleLMap lambda (f :&& g) = scaleLMap lambda f :&& scaleLMap lambda g

-- | Fancy category instance-to-be
compose
  :: k \> a ~> b
  -> k \>      b ~> c
  -> k \> a ~>      c
compose (Dot x) (Dot k) = Dot (x >% k)
compose (f :&& g) (Dot (a, b)) =
  compose f (Dot a) `addLinearMap` compose g (Dot b)
compose x@(Dot _)   (f :&& g) = compose x f :&& compose x g
compose x@(_ :&& _) (f :&& g) = compose x f :&& compose x g

-- Constrained arrow-like functions

compFst
  :: ( VectorSpace'     k a
     , DotProductSpace' k b
     , DotProductSpace' k c
     ) => k \>  a     ~> c
       -> k \> (a, b) ~> c
compFst (Dot a)   = Dot (a, zero)
compFst (f :&& g) = compFst f :&& compFst g

compSnd
  :: ( DotProductSpace' k a
     , VectorSpace'     k b
     , DotProductSpace' k c
     ) => k \>     b  ~> c
       -> k \> (a, b) ~> c
compSnd (Dot a)   = Dot (zero, a)
compSnd (f :&& g) = compSnd f :&& compSnd g

tensorProductLinear
  :: ( DotProductSpace' k a
     , DotProductSpace' k b
     , DotProductSpace' k c
     , DotProductSpace' k d
     ) => k \>  a     ~>  b
       -> k \>     c  ~>     d
       -> k \> (a, c) ~> (b, d)
tensorProductLinear f g = compFst f :&& compSnd g

------------------------------------------------------------------------------
-- Show instances
-- (don't look)
------------------------------------------------------------------------------

-- "How many layers of law-breaking are you on?"
instance (Show a) => Show (k \> a ~> k) where
  show (Dot a) = show a
  show _ = error "impossible"

-- "like, maybe polymorphically recursively many, my dude"
instance ( Show (k \> a ~> b)
         , Show (k \> a ~> c)
         , Show a
         ) => Show (k \> a ~> (b, c)) where
  show (Dot a) = show a
  show (f :&& g) = show f ++ "+\n" ++ show g

----------------------------------------------------------------------
-- Algebraic structures on linear map types
----------------------------------------------------------------------

----------------------------------------------------------------------
-- Additive and multiplicative operations and identity elements
----------------------------------------------------------------------

-- | Linear maps between the same pair of spaces can always be added.
instance Magma Add (k \> a ~> b) where
  binaryOp _ = addLinearMap

-- | Additive neutral element, base case
instance (Monoid Add a, DotProductSpace' k a) =>
         Neutral Add (k \> a ~> k) where
  neutral _ = Dot zero

-- | Additive neutral element, recursive step
instance ( Neutral Add (k \> a ~> b)
         , Neutral Add (k \> a ~> c)
         , VectorSpace' k a
         , DotProductSpace' k b
         , DotProductSpace' k c
         ) => Neutral Add (k \> a ~> (b, c)) where
  neutral p = neutral p :&& neutral p

-- | Only "square matrices" have a "monomorphic" (in some sense) multiplication.
instance Magma Mul (k \> a ~> a) where
  binaryOp _ = compose

-- | Multiplicative neutral element, base case
instance (Monoid Mul a, DotProductSpace' k a) =>
         Neutral Mul (k \> a ~> k) where
  neutral _ = Dot one

-- | Multiplicative neutral element, recursive step
instance ( Monoid Add (k \> a ~> b)
         , Monoid Add (k \> a ~> c)
         , Monoid Mul (k \> a ~> b)
         , Monoid Mul (k \> a ~> c)
         , Monoid Add (k \> a ~> (b, c))
         , VectorSpace' k a
         , DotProductSpace' k b
         , DotProductSpace' k c
         ) =>
         Neutral Mul (k \> a ~> (b, c)) where
  neutral _ = neutral MulP :&& neutral AddP
            + neutral AddP :&& neutral MulP

-----------------------------------------------------------------
-- Semigroup and monoid structures
-----------------------------------------------------------------

-- | Trivial additive semigroup structure from `Monoid` and `Neutral`
-- instances
instance Semigroup Add (k \> a ~> b)

-- | Additive monoid, base case
instance ( DotProductSpace' k a
         ) => Monoid Add (k \> a ~> k)

-- | Additive monoid, recursive step
instance ( VectorSpace' k a
         , DotProductSpace' k b
         , DotProductSpace' k c
         , Monoid Add (k \> a ~> b)
         , Monoid Add (k \> a ~> c)
         ) => Monoid Add (k \> a ~> (b, c))

-- instance Semigroup Mul (k \> a ~> a)

-- instance (Monoid Mul a, DotProductSpace' k a) =>
--          Monoid Mul (k \> a ~> a)

-- instance ( Monoid Add (a ~> b /> k)
--          , Monoid Mul (a ~> c /> k)
--          , VectorSpace' k a
--          , DotProductSpace' k b
--          , DotProductSpace' k c
--          ) => Monoid Mul (a ~> (b, c) /> k) where
