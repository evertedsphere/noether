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
module Noether.Algebra.Linear.Naperian where

import           Prelude                       hiding (length, lookup,
                                                replicate, zipWith)
import qualified Prelude

import           Control.Applicative           (liftA2)
import qualified Data.IntMap                   as IntMap
import           Data.Kind                     (Constraint, Type)
import           Data.List                     (intercalate)
import qualified GHC.Exts                      as L (IsList (..))
import           GHC.Prim
import           GHC.TypeLits

import qualified Data.Vector                   as Vector

import           Data.Foldable                 (toList)

import           Data.Maybe

import           Noether.Algebra.Linear.Vector

--------------------------------------------------------------------------------
-- Naperian functors

-- | Naperian functors.

-- A useful way of thinking about a Naperian functor is that if we have a value
-- of type @v :: f a@ for some @'Naperian' f@, then we can think of @f a@ as a
-- bag of objects, with the ability to pick out the @a@ values inside the bag,
-- for each and every @a@ inside @f@. For example, in order to look up a value
-- @a@ inside a list @[a]@, we could use a function @[a] -> Int -> a@, which is
-- exactly @'(Prelude.!!)'@
--
-- The lookup function acts like a logarithm of the @'Functor' f@. Intuitively,
-- a Haskell function @f :: a -> b@ acts like the exponential @b^a@ if we intuit
-- types as an algebraic quantity. The logarithm of some value @x = b^a@ is
-- defined as @log_b(x) = a@, so given @x@ and a base @b@, it finds the exponent
-- @a@. In Haskell terms, this would be like finding the input value @a@ to a
-- function @f :: a -> b@, given a @b@, so it is a reverse mapping from the
-- outputs of @f@ back to its inputs.
--
-- A @'Naperian'@ functor @f@ is precisely a functor @f@ such that for any value
-- of type @f a@, we have a way of finding every single @a@ inside.
class Functor f => Naperian f where
  {-# MINIMAL lookup, (tabulate | positions) #-}

  -- | The \"logarithm\" of @f@. This type represents the 'input' you use to
  -- look up values inside @f a@. For example, if you have a list @[a]@, and
  -- you want to look up a value, then you use an @'Int'@ to index into
  -- the list. In this case, @'Log' [a] = Int@. If you have a type-bounded
  -- Vector @'Vector' (n :: 'Nat') a@, then @'Log' ('Vector' n)@ is the
  -- range of integers @[0..n-1]@ (represented here as @'Finite' n@.)
  type Log f

  -- | Look up an element @a@ inside @f a@. If you read this function type in
  -- english, it says \"if you give me an @f a@, then I will give you a
  -- function, so you can look up the elements of @f a@ and get back an @a@\"
  lookup :: f a -> (Log f -> a)

  -- | Tabulate a @'Naperian'@. This creates @f a@ values by mapping the logarithm
  -- of @f@ onto every \"position\" inside @f a@
  tabulate :: (Log f -> a) -> f a
  tabulate h = fmap h positions

  -- | Find every position in the \"space\" of the @'Naperian' f@.
  positions :: f (Log f)
  positions = tabulate id

-- | The transposition of two @'Naperian'@ functors @f@ and @g@.
transpose :: (Naperian f, Naperian g) => f (g a) -> g (f a)
transpose = tabulate . fmap tabulate . flip . fmap lookup . lookup

instance Naperian Pair where
  type Log Pair = Bool
  lookup (Pair x y) b = if b then y else x

  positions = Pair False True

instance KnownNat n => Naperian (Vector n) where
  type Log (Vector n) = Finite n

  lookup    = index
  positions = viota

--------------------------------------------------------------------------------
-- Dimensions

class (Applicative f, Naperian f, Traversable f) => Dimension f where
  size :: f a -> Int
  size = Prelude.length . toList

instance               Dimension Pair       where size = const 2
instance KnownNat n => Dimension (Vector n) where size = length

inner :: (Num a, Dimension f) => f a -> f a -> a
inner xs ys = sum (liftA2 (*) xs ys)

matrix :: ( Num a
          , Dimension f
          , Dimension g
          , Dimension h
          ) => f (g a)
            -> g (h a)
            -> f (h a)
matrix xss yss = liftA2 (liftA2 inner) (fmap pure xss) (pure (transpose yss))
