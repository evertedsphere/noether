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

module Noether.Equality where

import           Data.Coerce
import           Data.Kind    hiding (type (*))
import           Data.Proxy
import           GHC.Exts
import           GHC.Prim
import           GHC.TypeLits

import           Prelude      hiding (Eq, (==))
import qualified Prelude

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

    The Eq a/EquateAs s a trick is straight off the GHC wiki, as I said, although
    we can now use proxies instead of slinging 'undefined's around :)
-}

class Eq a where
  (==) :: a -> a -> EquateResult' a

instance (EquateAs s a, s ~ Equality a) => Eq a where
  (==) = equateAs (proxy# :: Proxy# s)

{-| An instance of this class defines a way to equate two terms of
    a given type according to a given "strategy" 's'.
-}

class EquateAs s a where
  equateAs :: Proxy# s -> a -> a -> EquateResult s a

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

-- Prelude equality.

data PreludeEq

type instance EquateResult PreludeEq a = Bool

instance (Prelude.Eq a) => EquateAs PreludeEq a where
  equateAs _ = (Prelude.==)

{- "Numeric" equality.

   This is obviously the same as Prelude equality. It makes for another example,
   that's all.
-}

data Numeric

type instance EquateResult Numeric a = Bool

instance (Prelude.Eq a, Num a) => EquateAs Numeric a where
  equateAs _ = (Prelude.==)

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
      p = proxy# :: Proxy# Approximate

{- Ideally, all equality strategies with a 'Bool' equality result could've been
   quantified over here, but I don't see how that can be done without replacing
   EquateResult by a fundep of some sort or ending up with un-unifiable constraints
   via the use of a type-level if, where the latter would lead to scary errors that
   look like:

   "No instance for <two pages of really helpfully expanded type synonyms>"
-}

data Common a

type instance EquateResult (Common Numeric) (a, a) = Bool

instance ( EquateAs Numeric a
         , EquateAs Numeric a
         ) => EquateAs (Common Numeric) (a, a) where

  equateAs _ (x, y) (x', y') = equateAs p x x' && equateAs p y y'
    where
      p = proxy# :: Proxy# Numeric

type instance EquateResult (Common PreludeEq) (a, a) = Bool

instance ( EquateAs PreludeEq a
         , EquateAs PreludeEq a
         ) => EquateAs (Common PreludeEq) (a, a) where

  equateAs _ (x, y) (x', y') = equateAs p x x' && equateAs p y y'
    where
      p = proxy# :: Proxy# PreludeEq

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
      pl = proxy# :: Proxy# l
      pr = proxy# :: Proxy# r

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
  equateAs _ x y = x `div` n' Prelude.== y `div` n'
    where
      n' = fromInteger $ natVal' (proxy# :: Proxy# n)

{-| Lightweight equality for newtypes using 'Coercible' from 'Data.Coerce'.

    This is so, so wonderful. (Well, now that the complaints about differing
    representations have gone away, anyway.)
-}

data CoerceFrom a s
type CoerceFrom' a = CoerceFrom a (Equality a)

type instance EquateResult (CoerceFrom a s) b = EquateResult s a

instance ( EquateAs s a
         , Coercible b a
         ) => EquateAs (CoerceFrom a s) b where
  equateAs _ x y = equateAs p (coerce x :: a) (coerce y :: a)
    where
      p = proxy# :: Proxy# s

