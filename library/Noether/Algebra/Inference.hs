{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module Noether.Algebra.Inference
  ( Synergise
  , Infer
  , Prim
  , DerivedFrom
  , Strategy
  , module TypeFu.Map
  ) where

import           Noether.Lemmata.TypeFu
import           Noether.Lemmata.TypeFu.Map as TypeFu.Map

-- bahahahaha
data Synergise (a :: [k])

type family Strategy (t :: k) (a :: Type) (hint :: Symbol) :: Type

type family Infer_ (t :: k) (a :: Type) (hint :: [Symbol]) :: [Type] where
  Infer_ t a '[] = '[]
  Infer_ t a (x : xs) = (x := Strategy t a x) : Infer_ t a xs

type Infer (t :: k) a (h :: [Symbol]) = Synergise (Nub (Sort (Infer_ t a h)))

-- Tags

data DerivedFrom (a :: k)

data Prim
