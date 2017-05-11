{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

{-| Flexible, extensible notions of equality supporting "custom deriving strategies".

    There are no incoherent or even overlapping instances anywhere here.
    The ideas are based off of the "Advanced Overlap" page on the Haskell wiki:
    <https://wiki.haskell.org/GHC/AdvancedOverlap>, and were inspired by
    the observation that the overlapping instances there could be completely
    replaced with not-necessarily-closed type families.

    This last point is crucial for Noether, which aspires to be a library that
    people can use for their own work. The closed-TF approach of declaring how
    a couple standard types are compared, putting a catch-all case after
    those to handle everything else (all in the core library), and then calling it a day
    isn't really sensible, then, for obvious reasons.

    I have some prototype Oleg-style "guided resolution" in development that seems to
    be promising, and I think this approach, together with the former, can be used
    to handle (instances of typeclasses representing) algebraic structures on types
    without the incoherent nonsense in place currently.
-}

module ExtensibleEquality where

import           Data.Coerce
import           Data.Proxy
import           GHC.Exts
import           GHC.TypeLits
import           Prelude


{-| This represents the unique "equality strategy" to be used for 'a'.

    There may be many different notions of equality that can be applied to a
    particular type, and instances of the 'Equality' family are used to disambiguate
    those by specifying which one to use.
-}

type family Equality a

{-| Different notions of equality can have different "results". For instance,
    standard Eq-style "tired" equality returns Bool values, whereas a more
    numerically "wired" implementation for floating-point numbers could instead
    use tolerances/"epsilons" to compare things.

    This is reminiscent of subhask-ish things (in particular, the all-pervasive
    Logic type family).
-}

type family EquateResult s a

type EquateResult' a = EquateResult (Equality a) a
type EquateAs' a = EquateAs (Equality a) a

{-| This is the user-facing 'Eq' replacement class.

    The Equate a/EquateAs s a trick is straight off the GHC wiki, as I said, although
    we can now use proxies instead of slinging 'undefined's around :)
-}

class Equate a where
  (===) :: a -> a -> EquateResult' a

instance (EquateAs s a, s ~ Equality a) => Equate a where
  (===) = equateAs (Proxy :: Proxy s)

{-| An instance of this class defines a way to equate two terms of
    a given type according to a given "strategy" 's'.
-}

class EquateAs s a where
  equateAs :: proxy s -> a -> a -> EquateResult s a

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

-- Prelude equality.

data PreludeEq

type instance EquateResult PreludeEq a = Bool

instance (Eq a) => EquateAs PreludeEq a where
  equateAs _ = (==)

-- "Numeric" equality.

data Numeric

type instance EquateResult Numeric a = Bool

instance (Eq a, Num a) => EquateAs Numeric a where
  equateAs _ = (==)

{- "Approximate" equality defined only up to an epsilon.
   (`reflection` could be considered if one wanted to defer the choice of tolerance.)
-}

data Approximate

type instance EquateResult Approximate a = a -> Bool

instance (Num a, Ord a) =>
         EquateAs Approximate a where
  equateAs _ x y epsilon = abs (x - y) < epsilon

-- Maybe this is a good time to learn generics?

type instance EquateResult (Common Approximate) (a, a) = a -> Bool

instance (EquateAs Approximate a) =>
         EquateAs (Common Approximate) (a, a) where
  equateAs _ (x,y) (x',y') eps = equateAs p x x' eps && equateAs p y y' eps
    where
      p = Proxy :: Proxy Approximate

{- Ideally, all equality strategies with a 'Bool' equality result
   could've been quantified over here, but I don't see how that can
   be done without replacing EquateResult by a fundep of some sort
   or ending up with un-unifiable constraints via the use of a type-level if,
   where the latter would lead to scary "No instance for <two pages of type synonym
   expansions>" errors.
-}

data Common a

type instance EquateResult (Common Numeric) (a, a) = Bool

instance ( EquateAs Numeric a
         , EquateAs Numeric a
         ) => EquateAs (Common Numeric) (a, a) where

  equateAs _ (x, y) (x', y') = equateAs p x x' && equateAs p y y'
    where
      p = Proxy :: Proxy Numeric

type instance EquateResult (Common PreludeEq) (a, a) = Bool

instance ( EquateAs PreludeEq a
         , EquateAs PreludeEq a
         ) => EquateAs (Common PreludeEq) (a, a) where

  equateAs _ (x, y) (x', y') = equateAs p x x' && equateAs p y y'
    where
      p = Proxy :: Proxy PreludeEq

{-| The 'Composite' strategy just uses the canonical strategies on each
    "slot" of the tuple and returns a tuple of results.

    It's ... sort of lazy.
-}

data Composite a b

type instance EquateResult (Composite l r) (a, b) =
     (EquateResult l a, EquateResult r b)

instance (EquateAs l a, EquateAs r b) =>
         EquateAs (Composite l r) (a, b) where
  equateAs _ (x,y) (x',y') = (equateAs pl x x', equateAs pr y y')
    where
      pl = Proxy :: Proxy l
      pr = Proxy :: Proxy r

{- You can always define one-off 'Explicit' equality strategies.

   If I can implement guided instance selection robustly, these can
   be expected to have the highest priority (unless one thinks of attaching
   priorities to the strategies themselves, like fixity annotations! TODO).
-}

data Explicit (s :: k)

{- For instance, consider comparing integers for equality modulo
   some number.
-}

data Modulo (n :: Nat)

type instance EquateResult (Explicit (Modulo n)) Int = Bool

instance KnownNat n => EquateAs (Explicit (Modulo n)) Int where
  equateAs _ x y = x `div` n' == y `div` n'
    where
      n' = fromInteger $ natVal (Proxy :: Proxy n)

{-| Lightweight equality for newtypes using 'Coercible' from 'Data.Coerce'.

    This is so, so wonderful. (Well, now that the complaints about differing
    representations have gone away, anyway.)
-}

data CoerceFrom s a
type CoerceFrom' a = CoerceFrom (Equality a) a

type instance EquateResult (CoerceFrom s a) b = EquateResult s a

instance ( EquateAs s a
         , Coercible b a
         ) => EquateAs (CoerceFrom s a) b where
  equateAs _ x y = equateAs p (coerce x :: a) (coerce y :: a)
    where
      p = Proxy :: Proxy s

--------------------------------------------------------------------------------
-- Usage
--------------------------------------------------------------------------------

{- Library definitions would probably look like this.

   (They might even be put in Backpack signature files or something, for, e.g.
   a swap-in numerically sensible set of equality strategies that uses approximate
   equality for everything floating-ish.)

   These type instance definitions can be thought of as fine-grained applications of
   custom deriving strategies, as alluded to in the opening wall of text. So we have
   'Numeric', but also 'Composite Approximate Numeric', and so on. (More generics?)
-}

-- This might be read as "deriving instance Equality Int using strategy Numeric".
type instance Equality Int = Numeric

testInt = 0 === (1 :: Int)

type instance Equality Double = Approximate

testDouble = (t eps1, t eps2)
  where
    t = (0.0 === (0.01 :: Double))
    eps1 = 0.001
    eps2 = 0.1

{- What follows are the specifications of (unique!) equality strategies for
   a couple of types. A user would only interact with this bit, ideally, to define
   equality for her own types.

   (Note that every test* type hereafter can be inferred, "obviously".)
-}

{-| 'Common' 'Approximate' is a strategy that uses the same epsilon for both
    slots of the tuple, going

    (a -> Bool, a -> Bool) ~> a -> (Bool, Bool) ~> a -> Bool
-}
type instance Equality (Double, Double) = Common Approximate

test1 :: Double -> Bool
test1 = lhs === rhs
  where
    lhs, rhs :: (Double, Double)
    lhs = (2.0, 2.0)
    rhs = (2.00001, 2.0)

{- Suppose I want a newtype that is equated differently.
   For instance, consider a newtype-wrapped Double that compares
   according to Prelude equality, not the funky tolerance-ish thing
   above.

   In defining it,  I can skip making it support the operations that my choice
   of equality strategy requires. The use of 'PreludeEq' demands an 'Eq' constraint,
   but 'Dbl' does not need to derive that.

   FIXME: Maybe the type family should make this available? The Advanced
   Overlap page says that getting the class instances and the type instances
   to agree is "just something you'll have to do", but does ConstraintKinds let
   us hack around that now?
-}

newtype Dbl = Dbl Double

-- Now I can simply coerce the equality on the base type to the newtype.

type instance Equality Dbl = CoerceFrom PreludeEq Double

test2 :: Bool
test2 = Dbl 2.0 === Dbl 2.01

{- In case of 'Eq' on a newtype-wrapped Prelude numeric type,
   this is a parlor trick at best, but not having to "derive" Num
   (or write a one-off partial implementation) is awesome:
-}
newtype Dbl' = Dbl' Double

-- et .. magic!
type instance Equality Dbl' = CoerceFrom Numeric Double

test3 :: Bool
test3 = Dbl' 2.0 === Dbl' 2.01

{- (I'm intentionally using 'Composite' instead of a tuple to leave that option open
   for auto-deriving shenanigans.)
-}

type instance Equality (Dbl, Dbl') = Composite (Equality Dbl) (Equality Dbl')

test4 :: (Bool, Bool)
test4 = (Dbl 2, Dbl' 2) === (Dbl 2.001, Dbl' 2)

-- Let's try the Z/n equality we defined above.

-- As before, define the newtype...

newtype Mod (n :: Nat) = Mod Int

-- "derive" the equality strategy...

type instance Equality (Mod n) = CoerceFrom (Explicit (Modulo n)) Int

--- and profit!

test5 :: Bool
test5 = a === b
  where
    a, b :: Mod 7
    a = Mod 3
    b = Mod 24
