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
module Noether.Algebra.Linear.Hyper where

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
import           Noether.Algebra.Linear.Naperian

--------------------------------------------------------------------------------
-- Hyper-dimensional stuff

-- | Arbitrary-rank Hypercuboids, parameterized over their dimension.
data Hyper :: [Type -> Type] -> Type -> Type where
  Scalar :: a -> Hyper '[] a
  Prism  :: (Dimension f, Shapely fs) => Hyper fs (f a) -> Hyper (f : fs) a

point :: Hyper '[] a -> a
point (Scalar a) = a

crystal :: Hyper (f : fs) a -> Hyper fs (f a)
crystal (Prism x) = x

instance Show a => Show (Hyper fs a) where
  show = showHyper . fmap show where
    showHyper :: Hyper gs String -> String
    showHyper (Scalar s) = s
    showHyper (Prism x)  = showHyper (fmap (ppVector . toList) x)

{--
class HyperLift f fs where
  hyper :: (Shapely fs, Dimension f) => f a -> Hyper (f : fs) a

instance HyperLift f '[] where
  hyper = Prism . Scalar

instance (Shapely fs, HyperLift f fs) => HyperLift f (f : fs) where
  hyper = Prism . (\x -> (hyper $ _ x))
--}

class Shapely fs where
  hreplicate :: a -> Hyper fs a
  hsize      :: Hyper fs a -> Int

instance Shapely '[] where
  hreplicate a = Scalar a
  hsize        = const 1

instance (Dimension f, Shapely fs) => Shapely (f : fs) where
  hreplicate a = Prism (hreplicate (pure a))
  hsize (Prism x) = size (first x) * hsize x

instance Functor (Hyper fs) where
  fmap f (Scalar a) = Scalar (f a)
  fmap f (Prism x)  = Prism (fmap (fmap f) x)

instance Shapely fs => Applicative (Hyper fs) where
  pure  = hreplicate
  (<*>) = hzipWith ($)

hzipWith :: (a -> b -> c) -> Hyper fs a -> Hyper fs b -> Hyper fs c
hzipWith f (Scalar a) (Scalar b) = Scalar (f a b)
hzipWith f (Prism x)  (Prism y)  = Prism (hzipWith (azipWith f) x y)

first :: Shapely fs => Hyper fs a -> a
first (Scalar a) = a
first (Prism x)  = head (toList (first x))

-- | Generalized transposition over arbitrary-rank hypercuboids.
transposeH :: Hyper (f : (g : fs)) a
           -> Hyper (g : (f : fs)) a
transposeH (Prism (Prism x)) = Prism (Prism (fmap transpose x))

-- | Fold over a single dimension of a Hypercuboid.
foldrH :: (a -> a -> a) -> a -> Hyper (f : fs) a -> Hyper fs a
foldrH f z (Prism x) = fmap (foldr f z) x

-- | Lift an unary function from values to hypercuboids of values.
unary :: Shapely fs => (a -> b) -> (Hyper fs a -> Hyper fs b)
unary = fmap

-- | Lift a binary function from values to two sets of hypercuboids, which can
-- be aligned properly.
binary :: ( Compatible fs gs
          , Max fs gs ~ hs
          , Alignable fs hs
          , Alignable gs hs
          ) => (a -> b -> c)
            -> Hyper fs a
            -> Hyper gs b
            -> Hyper hs c
binary f x y = hzipWith f (align x) (align y)

up :: (Shapely fs, Dimension f) => Hyper fs a -> Hyper (f : fs) a
up = Prism . fmap pure

-- | Generalized, rank-polymorphic inner product.
innerH :: ( Max fs gs ~  (f : hs)
          , Alignable fs (f : hs)
          , Alignable gs (f : hs)
          , Compatible fs gs
          , Num a
          ) => Hyper fs a
            -> Hyper gs a
            -> Hyper hs a
innerH xs ys = foldrH (+) 0 (binary (*) xs ys)

-- | Generalized, rank-polymorphic matrix product.
matrixH :: ( Num a
           , Dimension f
           , Dimension g
           , Dimension h
           ) => Hyper '[ g, f ] a
             -> Hyper '[ h, g ] a
             -> Hyper '[ h, f ] a
matrixH x y = case (crystal x, transposeH y) of
  (xs, Prism (Prism ys)) -> hzipWith inner (up xs) (Prism (up ys))

--------------------------------------------------------------------------------
-- Alignment

class (Shapely fs, Shapely gs) => Alignable fs gs where
  align :: Hyper fs a -> Hyper gs a

instance Alignable '[] '[] where
  align = id

instance (Dimension f, Alignable fs gs) => Alignable (f : fs) (f : gs) where
  align (Prism x) = Prism (align x)

instance (Dimension f, Shapely fs) => Alignable '[] (f : fs) where
  align (Scalar a) = hreplicate a

type family Max (fs :: [Type -> Type]) (gs :: [Type -> Type]) :: [Type -> Type] where
  Max '[]      '[]      = '[]
  Max '[]      (f : gs) = f : gs
  Max (f : fs) '[]      = f : fs
  Max (f : fs) (f : gs) = f : Max fs gs

type CompatError a b
  =     'Text "Mismatched dimensions!"
  ':$$: 'Text "The dimension "
  ':<>: 'ShowType a
  ':<>: 'Text " can't be aligned with"
  ':$$: 'Text "the dimension "
  ':<>: 'ShowType b

type family Compatible (fs :: [Type -> Type]) (gs :: [Type -> Type]) :: Constraint where
  Compatible '[] '[]           = ()
  Compatible '[] (f : gs)      = ()
  Compatible (f : fs) '[]      = ()
  Compatible (f : fs) (f : gs) = Compatible fs gs
  Compatible a b               = TypeError (CompatError a b)

--------------------------------------------------------------------------------
-- Flattened, sparse Hypercuboids

elements :: Shapely fs => Hyper fs a -> [a]
elements (Scalar a) = [a]
elements (Prism a)  = concat (map toList (elements a))

data Flat fs a where
  Flat :: Shapely fs => Vector.Vector a -> Flat fs a

instance Functor (Flat fs) where
  fmap f (Flat v) = Flat (fmap f v)

instance Show a => Show (Flat fs a) where
  show = showHyper . fmap show where
    showHyper :: Flat gs String -> String
    showHyper (Flat v) = ppVector (toList v)

flatten :: Shapely fs => Hyper fs a -> Flat fs a
flatten hs = Flat (Vector.fromList (elements hs))

data Sparse fs a where
  Sparse :: Shapely fs => a -> IntMap.IntMap a -> Sparse fs a

unsparse :: forall fs a. Shapely fs => Sparse fs a -> Flat fs a
unsparse (Sparse e xs) = Flat (Vector.unsafeAccum (flip const) vs as)
  where
    as     = IntMap.assocs xs
    vs     = Vector.replicate l e
    l      = hsize (hreplicate () :: Hyper fs ())

--------------------------------------------------------------------------------
-- Examples

type Matrix n m v = Vector n (Vector m v)

example1 :: Int
example1 = inner v1 v2 where
  v1 = [ 1, 2, 3 ] :: Vector 3 Int
  v2 = [ 4, 5, 6 ] :: Vector 3 Int

example2 :: Matrix 2 2 Int
example2 = matrix m1 m2 where
  m1 = [ [ 1, 2, 3 ]
       , [ 4, 5, 6 ]
       ] :: Matrix 2 3 Int

  m2 = [ [ 9, 8 ]
       , [ 6, 5 ]
       , [ 3, 2 ]
       ] :: Matrix 3 2 Int

example3 :: Hyper '[] Int
example3 = innerH v1 v2 where
  v1 = Prism (Scalar [1, 2, 3]) :: Hyper '[Vector 3] Int
  v2 = Prism (Scalar [4, 5, 6]) :: Hyper '[Vector 3] Int

example4 :: Hyper '[Vector 2, Vector 2] Int
example4 = matrixH v1 v2 where
  x = [ [ 1, 2, 3 ]
      , [ 4, 5, 6 ]
      ] :: Matrix 2 3 Int

  y = [ [ 9, 8 ]
      , [ 6, 5 ]
      , [ 3, 2 ]
      ] :: Matrix 3 2 Int

  v1 = Prism (Prism (Scalar x)) :: Hyper '[Vector 3, Vector 2] Int
  v2 = Prism (Prism (Scalar y)) :: Hyper '[Vector 2, Vector 3] Int
