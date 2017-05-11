{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module ExtensibleEquality where

import           Data.Coerce
import           Data.Proxy
import           GHC.Exts
import           Prelude

data Numeric
data Approximate
data Explicit
data PreludeEq
data Common a
data Composite a b

type family Equality a
type family EquateResult s a

type EquateResult' a = EquateResult (Equality a) a
type EquateAs' a = EquateAs (Equality a) a

class Equate a where
  (===) :: a -> a -> EquateResult' a

instance (EquateAs (Equality a) a) => Equate a where
  (===) = equateAs (Proxy :: Proxy (Equality a))

-- | An instance of this class defines a way to equate two terms of
-- a given type according to a given "strategy" 's'.

class EquateAs s a where
  equateAs :: proxy s -> a -> a -> EquateResult s a

------------------------------------------------------
-- Instances
------------------------------------------------------

-- Prelude equality.

type instance EquateResult PreludeEq a = Bool

instance (Eq a) => EquateAs PreludeEq a where
  equateAs _ = (==)

-- "Numeric" equality.

type instance EquateResult Numeric a = Bool

instance (Eq a, Num a) => EquateAs Numeric a where
  equateAs _ = (==)

-- "Approximate" equality.

type instance EquateResult Approximate a = a -> Bool

instance (Num a, Ord a) =>
         EquateAs Approximate a where
  equateAs _ x y epsilon = abs (x - y) < epsilon

-- Maybe this is a good time to learn generics.

type instance EquateResult (Common Approximate) (a, a) = a -> Bool

instance (EquateAs Approximate a) =>
         EquateAs (Common Approximate) (a, a) where
  equateAs _ (x,y) (x',y') eps = equateAs p x x' eps && equateAs p y y' eps
    where
      p = Proxy :: Proxy Approximate

-- Ideally, all equality strategies with a 'Bool' equality result
-- could've been quantified over here.

type instance EquateResult (Common Numeric) (a, a) = Bool

instance (EquateAs Numeric a, EquateAs Numeric a) =>
         EquateAs (Common Numeric) (a, a) where
  equateAs _ (x,y) (x',y') = equateAs p x x' && equateAs p y y'
    where
      p = Proxy :: Proxy Numeric

type instance EquateResult (Common PreludeEq) (a, a) = Bool

instance (EquateAs PreludeEq a, EquateAs PreludeEq a) =>
         EquateAs (Common PreludeEq) (a, a) where
  equateAs _ (x,y) (x',y') = equateAs p x x' && equateAs p y y'
    where
      p = Proxy :: Proxy PreludeEq

-- The 'Composite' strategy just uses the canonical strategies on each
-- "slot" of the tuple and returns a tuple of results.

type instance EquateResult (Composite l r) (a, b) =
     (EquateResult l a, EquateResult r b)

instance (EquateAs l a, EquateAs r b) =>
         EquateAs (Composite l r) (a, b) where
  equateAs _ (x,y) (x',y') = (equateAs pl x x', equateAs pr y y')
    where
      pl = Proxy :: Proxy l
      pr = Proxy :: Proxy r

-- You can always define one-off 'Explicit' equality strategies.
-- If I can implement guided instance selection robustly, these can
-- be expected to have the highest priority.

type instance EquateResult Explicit Int = Bool
instance EquateAs Explicit Int where
  equateAs _ _ _ = False

-- Lightweight equality for newtypes using 'Coercible' from 'Data.Coerce'.
-- This is so, so wonderful. (Well, now that the complaints about differing
-- representations have gone away, anyway.)

data CoerceFrom s a
type CoerceFrom' a = CoerceFrom (Equality a) a

type instance EquateResult (CoerceFrom s a) b = EquateResult s a

instance ( EquateAs s a
         , Coercible b a
         ) => EquateAs (CoerceFrom s a) b where
  equateAs _ x y = equateAs p (coerce x :: a) (coerce y :: a)
    where
      p = Proxy :: Proxy s

-- Library definitions should look like this. (They might even be put in
-- signature files or something, for, e.g. a swap-in numerically sensible
-- set of equality strategies that uses approximate equality for everything
-- floating-ish.)

type instance Equality Int = Numeric

testInt = 0 === (1 :: Int)

type instance Equality Double = Approximate

-- | This is the specification of the (unique!) equality strategy.
-- A user would only interact with this bit, ideally.

-- | 'Common' 'Approximate' is a strategy that uses the same epsilon for
-- both slots of the tuple, going
-- (a -> Bool, a -> Bool) ~> a -> (Bool, Bool) ~> a -> Bool
type instance Equality (Double, Double) = Common Approximate

test1 :: Double -> Bool
test1 = lhs === rhs
  where
    lhs, rhs :: (Double, Double)
    lhs = (2.0, 2.0)
    rhs = (2.00001, 2.0)

-- | Suppose I want a newtype that is equated differently.
-- For instance, consider a newtype-wrapped Double that compares
-- according to Prelude equality, not the funky tolerance-ish thing
-- above.

-- First I define it, but I can skip making it support the operations
-- that my choice of equality strategy requires. 'PreludeEq' has an
-- 'Eq' constraint (maybe the type family should make this available?)
-- but 'Dbl' does not need to derive that.
newtype Dbl = Dbl Double

-- Now I can simply coerce the equality on the base type to the newtype.
type instance Equality Dbl = CoerceFrom PreludeEq Double

test2 :: Bool
test2 = Dbl 2.0 === Dbl 2.01

-- | In case of 'Eq' on a newtype-wrapped Prelude numeric type,
-- this is a parlor trick at best: but not having to "derive" Num
-- (or write a one-off partial implementation) is awesome:
newtype Dbl' = Dbl' Double

-- Magic!
type instance Equality Dbl' = CoerceFrom Numeric Double

test3 :: Bool
test3 = Dbl' 2.0 === Dbl' 2.01

-- (I'm intentionally using 'Composite' instead of a tuple to leave that option open
-- for auto-deriving shenanigans.)
type instance Equality (Dbl, Dbl') = Composite (Equality Dbl) (Equality Dbl')

test4 :: (Bool, Bool)
test4 = (Dbl 2, Dbl' 2) === (Dbl 2.001, Dbl' 2)
