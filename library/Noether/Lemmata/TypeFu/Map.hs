{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
module Noether.Lemmata.TypeFu.Map
  ( Nub
  , Sort
  , type (<+>)
  , TMap
  , TMap'
  , (:=)
  , type (\\)
  , Lookup
  , Combine
  , Member
  ) where

import           Noether.Lemmata.TypeFu
import           Prelude                    (Bool (..), Maybe (..))

import           Noether.Lemmata.TypeFu.Set hiding (Nub)

infixr 4 :=

data (:=) (k :: k1) (v :: k2)

type TMap m = Nub (Sort m)
type TMap' m = Sort m
type (<+>) m n = TMap (m ++ n)

type family Nub t where
  Nub '[] = '[]
  Nub '[x] = '[x]
  Nub ((k := v) : (k := w) : m) = Nub ((k := Combine v w) : m)
  Nub (x : y : s) = x : Nub (y : s)

type family Combine (a :: v) (b :: v) :: v

type family (m :: [k]) \\ (c :: Symbol) :: [k] where
  '[] \\ _ = '[]
  ((q := _) : m) \\ q = m \\ q
  (p : m) \\ q = p : (m \\ q)

type family Lookup (m :: [k']) (c :: k) :: Maybe v where
  Lookup '[] k = Nothing
  Lookup ((k := v) : _) k = Just v
  Lookup (_ : m) k = Lookup m k

type family Member (c :: k) (m :: [Type]) :: Bool where
  Member _ '[] = False
  Member k ((k := _) : _) = True
  Member k (_ : m) = Member k m

type instance Cmp (k :: Symbol) (k' :: Symbol) = CmpSymbol k k'
type instance Cmp (k := _) (k' := _) = CmpSymbol k k'
