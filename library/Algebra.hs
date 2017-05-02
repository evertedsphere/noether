{-# LANGUAGE UndecidableInstances #-}
module Algebra where

import           Lemmata hiding (Monoid, Semigroup, Semiring, fromInteger,
                          negate, one, zero, (*), (+), (-), (/))

import qualified Lemmata as L

class Semigroup g where
  infixl 6 +
  (+) :: g -> g -> g

class Semigroup g =>
      Monoid g where
  zero :: g

class Semigroup g =>
      Cancellative g where
  infixl 6 -
  (-) :: g -> g -> g

class (Cancellative g, Monoid g) =>
      Group g where
  negate :: g -> g
  negate g = zero - g

class Semigroup m => Abelian m

class (Abelian r, Monoid r) => Rg r where
  infixl 7 *
  (*) :: r -> r -> r

class (Abelian r, Monoid r) => Semiring r where
  one :: r

class (Rg r, Group r) => Rng r

class (Rng r, Semiring r) => Ring r where
    fromInteger :: Integer -> r

class Ring r => Field r where
    reciprocal :: r -> r
    reciprocal r = one/r

    infixl 7 /
    (/) :: r -> r -> r
    n/d = n * reciprocal d

    fromRational :: Rational -> r
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance Semigroup Int      where (+) = (L.+)
instance Semigroup Integer  where (+) = (L.+)
instance Semigroup Float    where (+) = (L.+)
instance Semigroup Double   where (+) = (L.+)
instance Semigroup Rational where (+) = (L.+)

instance Monoid Int      where zero = 0
instance Monoid Integer  where zero = 0
instance Monoid Float    where zero = 0
instance Monoid Double   where zero = 0
instance Monoid Rational where zero = 0

instance Cancellative Int      where (-) = (L.-)
instance Cancellative Integer  where (-) = (L.-)
instance Cancellative Float    where (-) = (L.-)
instance Cancellative Double   where (-) = (L.-)
instance Cancellative Rational where (-) = (L.-)

instance Group Int      where negate = L.negate
instance Group Integer  where negate = L.negate
instance Group Float    where negate = L.negate
instance Group Double   where negate = L.negate
instance Group Rational where negate = L.negate

instance Abelian Int
instance Abelian Integer
instance Abelian Float
instance Abelian Double
instance Abelian Rational

instance Rg Int      where (*) = (L.*)
instance Rg Integer  where (*) = (L.*)
instance Rg Float    where (*) = (L.*)
instance Rg Double   where (*) = (L.*)
instance Rg Rational where (*) = (L.*)

instance Semiring Int      where one = 1
instance Semiring Integer  where one = 1
instance Semiring Float    where one = 1
instance Semiring Double   where one = 1
instance Semiring Rational where one = 1

instance Rng Int
instance Rng Integer
instance Rng Float
instance Rng Double
instance Rng Rational

instance Ring Int where fromInteger = L.fromInteger
instance Ring Integer where fromInteger = L.fromInteger
instance Ring Float where fromInteger = L.fromInteger
instance Ring Double where fromInteger = L.fromInteger
instance Ring Rational where fromInteger = L.fromInteger

instance Field Float where (/) = (L./)
instance Field Double where (/) = (L./)
instance Field Rational where (/) = (L./)

instance Semigroup b => Semigroup (a -> b) where
  f + g = \a -> f a + g a

instance Monoid b => Monoid (a -> b) where
  zero = \_ -> zero

instance Cancellative b => Cancellative (a -> b) where
  f - g = \a -> f a - g a

instance Group b => Group (a -> b) where
  negate f = negate . f

instance Abelian b => Abelian (a -> b)

instance Rg b => Rg (a -> b) where
  f * g = \a -> f a * g a

instance Semiring b => Semiring (a -> b) where
  one = \_ -> one

type ZZ = Integer
type QQ = Rational
