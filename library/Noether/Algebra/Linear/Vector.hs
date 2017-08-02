{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Noether.Algebra.Linear.Vector where

import           Prelude             hiding (length, lookup, replicate, zipWith)
import qualified Prelude

import           Control.Applicative (liftA2)
import qualified Data.IntMap         as IntMap
import           Data.Kind           (Constraint, Type)
import           Data.List           (intercalate)
import qualified GHC.Exts            as L (IsList (..))
import           GHC.Prim
import           GHC.TypeLits

import qualified Data.Vector         as Vector

import           Data.Foldable       (toList)

import           Data.Maybe

--------------------------------------------------------------------------------
-- Miscellaneous

-- | The finite set of type-bounded Naturals. A value of type @'Fin' n@ has
-- exactly @n@ inhabitants, the natural numbers from @[0..n-1]@.
data Finite :: Nat -> Type where
  Fin :: Int -> Finite n
  deriving (Eq, Show)

-- | Create a type-bounded finite number @'Fin' n@ from a runtime integer,
-- bounded to a statically known limit. If the input value @x > n@, then
-- @'Nothing'@ is returned. Otherwise, returns @'Just' (x :: 'Fin' n)@.
finite :: forall n. KnownNat n => Int -> Maybe (Finite n)
finite x =
  if x > y
    then Nothing
    else Just (Fin x)
  where
    y = fromIntegral (natVal' (proxy# :: Proxy# n))

-- | \"'Applicative' zipping\".
azipWith :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
azipWith h xs ys = pure h <*> xs <*> ys

-- | Format a vector to make it look nice.
ppVector :: [String] -> String
ppVector xs = "<" ++ intercalate "," xs ++ ">"

--------------------------------------------------------------------------------
-- Pairs

-- | The cartesian product @'a' ✕ 'a'@, equivalent to @(a, a)@.
data Pair a = Pair a a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Applicative Pair where
  pure a = Pair a a
  Pair k g <*> Pair a b = Pair (k a) (g b)

--------------------------------------------------------------------------------
-- Vectors

newtype Vector (n :: Nat) a = Vector (Vector.Vector a)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (Vector n a) where
  show = ppVector . map show . toList

instance KnownNat n => Applicative (Vector n) where
  pure  = replicate
  (<*>) = zipWith ($)

instance (KnownNat n, Traversable (Vector n)) => L.IsList (Vector n a) where
  type Item (Vector n a) = a
  toList = Data.Foldable.toList

  fromList xs = fromMaybe (error "Demanded vector of a list that wasn't the proper length") (fromList xs)

tail :: Vector (n + 1) a -> Vector n a
tail (Vector v) = Vector (Vector.tail v)

fromList :: forall n a. KnownNat n => [a] -> Maybe (Vector n a)
fromList xs =
  if Prelude.length xs == sz
    then Nothing
    else Just (Vector $ Vector.fromList xs)
  where
    sz = fromIntegral (natVal' (proxy# :: Proxy# n)) :: Int

zipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWith f (Vector a) (Vector b) = Vector (Vector.zipWith f a b)

length :: forall n a. KnownNat n => Vector n a -> Int
length _ = fromIntegral $ natVal' (proxy# :: Proxy# n)

replicate :: forall n a. KnownNat n => a -> Vector n a
replicate v = Vector (Vector.replicate sz v) where
  sz = fromIntegral (natVal' (proxy# :: Proxy# n)) :: Int

index :: Vector n a -> Finite n -> a
index (Vector v) (Fin n) = (Vector.!) v n

viota :: forall n. KnownNat n => Vector n (Finite n)
viota = Vector (fmap Fin (Vector.enumFromN 0 sz)) where
  sz = fromIntegral (natVal' (proxy# :: Proxy# n)) :: Int
