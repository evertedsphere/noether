{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Noether.Algebra.Linear.GADTMaps where

import           Noether.Algebra.Single
import           Noether.Algebra.Multiple
import           Noether.Algebra.Actions
import           Noether.Algebra.Linear.Module

import           Prelude         hiding (Monoid, fromInteger, negate, recip,
                                  (*), (+), (-), (/))
import Noether.Lemmata.TypeFu

-------------------------------------------------------------------------
-- FIXME: This module is broken, as it relies on the old encoding of 
-- the algebraic structures.

-- Linear maps in the style of Conal Elliott
-- from <http://conal.net/blog/posts/reimagining-matrices>,
-- adapted for compatibility with All The Polymorphism™.
-- Likely extremely nonperformant, but definitely pretty and a good place
-- to test the abstractions developed so far.
-------------------------------------------------------------------------

infixr 1 \>
infixr 0 ~>

type (&) a b = (a, b)
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

data (\>) :: (Type -> Type -> Type -> Type) where
  Dot
    :: DotProductSpace' k a
    => a
    -> k \> a ~> k
  (:&&)
    ::
     ( VectorSpace' k a
     , DotProductSpace' k b
     , DotProductSpace' k c
     ) => k \> a ~> b
       -> k \> a ~>     c
       -> k \> a ~> b & c

-- A couple exercises of the syntax:

-- | A π/2 counterclockwise rotation in R^2.
rotate90 :: Double \> Double & Double ~> Double & Double
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
     ) => k \> a     ~> c
       -> k \> a & b ~> c
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
     ) => k \> a     ~> b
       -> k \>     c ~>     d
       -> k \> a & c ~> b & d
tensorProductLinear f g = compFst f :&& compSnd g

------------------------------------------------------------------------------
-- Show instances
-- (don't look)
------------------------------------------------------------------------------

-- "How many layers of law-breaking are you on?"
instance (Show a) => Show (k \> a ~> k) where
  show (Dot a) = show a
  show _ = error "impossible"

-- "like, maybe polymorphically recursively* many, my dude"
instance ( Show (k \> a ~> b)
         , Show (k \> a ~> c)
         , Show a
         ) => Show (k \> a ~> (b, c)) where
  show (Dot a) = show a
  show (f :&& g) = show f ++ "\n" ++ show g

-- *maybe not exactly

----------------------------------------------------------------------
-- Algebraic structures on linear map types
----------------------------------------------------------------------

----------------------------------------------------------------------
-- Operations and identity elements
----------------------------------------------------------------------

-- Addition

-- | Linear maps between the same pair of spaces can always be added.
instance Magma Add (k \> a ~> b) where
  binaryOp _ = addLinearMap

-- | Additive neutral element, base case
instance (Monoid Add a, DotProductSpace' k a) =>
         Neutral Add (k \> a ~> k) where
  neutral _ = Dot zero

-- | Additive neutral element, recursive step
instance {-# INCOHERENT #-}
         ( Neutral Add (k \> a ~> b)
         , Neutral Add (k \> a ~> c)
         , VectorSpace' k a
         , DotProductSpace' k b
         , DotProductSpace' k c
         ) => Neutral Add (k \> a ~> b & c) where
  neutral p = neutral p :&& neutral p

-- Multiplication

-- | Only "square matrices" have a "monomorphic" (in some sense) multiplication.
instance Magma Mul (k \> a ~> a) where
  binaryOp _ = compose

-- | Multiplicative neutral element, base case
instance (Monoid Mul a, DotProductSpace' k a) =>
         Neutral Mul (k \> a ~> k) where
  neutral _ = Dot one

-- | Multiplicative neutral element, recursive step
instance {-# INCOHERENT #-}
         ( Neutral Mul (k \> a ~> a)
         , Neutral Mul (k \> b ~> b)
         , DotProductSpace' k a
         , DotProductSpace' k b
         ) =>
         Neutral Mul (k \> a & b ~> a & b) where
  neutral _ = tensorProductLinear (neutral MulP) (neutral MulP)

-----------------------------------------------------------------
-- Algebraic structures
-----------------------------------------------------------------

-- | Trivial commutative additive semigroup structure
instance Semigroup   Add (k \> a ~> b)
instance Commutative Add (k \> a ~> b)

-- | Trivial multiplicative semigroup structure
instance Semigroup Mul (k \> a ~> a)

-- | Additive neutral element, base case
instance ( Neutral Add (k \> a ~> b)
         , Field' k
         ) => Cancellative Add (k \> a ~> b) where
  invert _ = scaleLMap (-one)

-- Monoid structures come for free

------------------------------------------------------------------
-- Almost-inner product spaces
-- These classes really need to be fixed.
------------------------------------------------------------------

instance (Field' k) =>
         LeftActs Add k  Add (k \> a ~> b) Mul YesLinear NotCompatible where
  leftAct _ _ _ = scaleLMap

instance (Field' k) =>
         RightActs Add k Add (k \> a ~> b) Mul YesLinear NotCompatible where
  rightAct _ _ _ = scaleLMap

instance (Field' k) =>
         LeftActs Mul k  Add (k \> a ~> b) Mul NotLinear YesCompatible where
  leftAct _ _ _ = scaleLMap

instance (Field' k) =>
         RightActs Mul k Add (k \> a ~> b) Mul NotLinear YesCompatible where
  rightAct _ _ _ = scaleLMap

instance (Field' k, Neutral Add (k \> a ~> b)) =>
         LeftModule' k (k \> a ~> b)

instance (Field' k, Neutral Add (k \> a ~> b)) =>
         RightModule' k (k \> a ~> b)

instance (Field' k, Neutral Add (k \> a ~> b)) =>
         Bimodule_ k (k \> a ~> b)

-- More exercises of the syntax

f' :: Ring' a => a -> a
f' x = x

g'
  :: Field' k
  => k \>
       k & (k & k) ~> k & (k & k)
  -> k \>
       k & (k & k) ~> k & (k & k)
g' = f'

ringTest :: Double \> Double & Double ~> Double & Double
ringTest = zero + rotate90 * rotate90

actionTest :: Double \> Double & Double ~> Double & Double
actionTest = (lambda %< rotate90) >% (dot @Double (lambda,lambda) (lambda, zero))
  where lambda :: Double
        lambda = 0.3

-- (-1.1102230246251565e-16,-1.1102230246251565e-16)
-- (1.1102230246251565e-16,-1.1102230246251565e-16)
-- mat :: (Double \> (Double, Double)) ~> (Double, Double)
mat = rotate (pi / 2) * rotate (-pi / 4) - rotate (pi / 4)

det
  :: forall v k.
     Field' k
  => k \> v ~> v
  -> k

det (Dot a) = a

det (Dot a :&& Dot b) = dot a b

-- det (f :&& g) = undefined

det (Dot (a, b) :&& Dot (c, d) :&& Dot (e, f))
  = b * det (Dot c :&& Dot e)
  - d * det (Dot a :&& Dot e)
  + f * det (Dot a :&& Dot c)

-- det ((f :&& g) :&& Dot ((al, ar), b)) = det f

det ((Dot (a, b) :&& Dot (c, d)) :&& (Dot (e, f) :&& Dot (g, h)))
    = det $
        Dot (det (Dot a :&& Dot c), det (Dot e :&& Dot g))
    :&& Dot (det (Dot f :&& Dot h), det (Dot b :&& Dot d))

-- det ((f :&& g) :&& (h :&& i)) =
