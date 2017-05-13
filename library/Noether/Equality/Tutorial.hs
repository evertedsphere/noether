{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}

module Noether.Equality.Tutorial where

import           GHC.TypeLits
import           Prelude          hiding (Eq, (==))

import           Noether.Equality

{- Library definitions would probably look like this.

   (They might even be put in Backpack signature files or something, for, e.g.
   a swap-in numerically sensible set of equality strategies that uses approximate
   equality for everything floating-ish.)

   These type instance definitions can be thought of as fine-grained applications of
   custom deriving strategies, as alluded to in the opening wall of text. So we have
   'Numeric', but also 'Composite Approximate Numeric', and so on. (More generics?)
-}

-- This might be read as
-- | deriving instance Eq Int using strategy Numeric
type instance Equality Int = Numeric

testInt = 0 == (1 :: Int)

-- | deriving instance Eq Double using strategy Approximate
type instance Equality Double = Approximate

testDouble = (t eps1, t eps2)
  where
    t = 0.0 == (0.01 :: Double)
    eps1 = 0.001
    eps2 = 0.1

-- And so on.

{- What follows are the specifications of (unique!) equality strategies for
   a couple of types. A user would only interact with this bit, ideally, to define
   equality for her own types.

   (Note that every test* type hereafter can be inferred, "obviously".)
-}

{-| 'Common' 'Approximate' is a strategy that uses the same epsilon for both
    slots of the tuple, going

    (a -> Bool, a -> Bool) ~> a -> (Bool, Bool) ~> a -> Bool

    Note that I can replace this with Common Numeric and have that work just fine,
    even though the concrete specification of equality on Double is Approximate.
    Defining the Equality instance does not throw away the information of the other
    possible equality tests, and bigger types like tuples and such can make use of
    any equality that the components support.
-}
type instance Equality (Double, Double) = Common Approximate

test1 :: Double -> Bool
test1 = lhs == rhs
  where
    lhs, rhs :: (Double, Double)
    lhs = (2.0, 2.0)
    rhs = (2.00001, 2.0)

{- Suppose I want a newtype that is equated differently.
   For instance, consider a newtype-wrapped Double that compares according to Prelude
   equality, not the funky tolerance-ish thing above.

   In defining it,  I can skip making it support the operations that my choice
   of equality strategy requires. The use of 'PreludeEq' demands an 'Eq' constraint,
   but 'Dbl' does not need to derive that.

   FIXME: Maybe the type family should make this available? The Advanced Overlap page
   says that getting the class instances and the type instances to agree is "just
   something you'll have to do", but does ConstraintKinds let us hack around that now?
-}

newtype Dbl = Dbl Double

-- Now I can simply coerce the equality on the base type to the newtype.

type instance Equality Dbl = CoerceFrom Double PreludeEq

test2 :: Bool
test2 = Dbl 2.0 == Dbl 2.01

{- In case of 'Eq' on a newtype-wrapped Prelude numeric type, this is a parlor trick
   at best, but not having to "derive" Num (or write a one-off partial implementation)
   is awesome:
-}
newtype Dbl' = Dbl' Double

-- et .. magic!
type instance Equality Dbl' = CoerceFrom Double Numeric

test3 :: Bool
test3 = Dbl' 2.0 == Dbl' 2.01

{- (I'm intentionally using 'Composite' instead of a tuple to leave that option open
   for auto-deriving shenanigans.)
-}

type instance Equality (Dbl, Dbl') = Composite (Equality Dbl) (Equality Dbl')

test4 :: (Bool, Bool)
test4 = (Dbl 2, Dbl' 2) == (Dbl 2.001, Dbl' 2)

-- Let's try the Z/n equality we defined above.

-- As before, define the newtype...

newtype Mod (n :: Nat) = Mod Int

-- "derive" the equality strategy...

type instance Equality (Mod n) = CoerceFrom Int (Explicit (Modulo n))

--- and profit!

test5 :: Bool
test5 = a == b
  where
    a, b :: Mod 7
    a = Mod 3
    b = Mod 24
