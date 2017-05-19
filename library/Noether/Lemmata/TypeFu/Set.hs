{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
module Noether.Lemmata.TypeFu.Set where

import           Noether.Lemmata.TypeFu
import           Prelude                (Bool (..), Ordering (..))

type TSet s = Nub (Sort s)
type TSet' s = Sort s

type family Cmp (a :: k) (b :: k) :: Ordering

type family (++) (x :: [k]) (y :: [k]) :: [k] where
  '[]       ++ xs = xs
  (x : xs) ++ ys = x : (xs ++ ys)

-- xor
type family (^^) (a :: Bool) (b :: Bool) :: Bool where
  x ^^ x = False
  _ ^^ _ = True

type family Nub t where
  Nub '[] = '[]
  Nub '[e] = '[e]
  Nub (e : e : s) = Nub (e : s)
  Nub (e : f : s) = e : Nub (f : s)

type family Sort (xs :: [k]) :: [k] where
  Sort '[] = '[]
  Sort (x : xs) = Sort (Filter False x xs) ++ '[x] ++ Sort (Filter True x xs)

type family Filter (f :: Bool) (p :: k) (xs :: [k]) :: [k] where
  Filter _ _ '[] = '[]
  Filter f p (x : xs) = IfCons (f ^^ (Cmp x p == LT)) x (Filter f p xs)

type family IfCons (pred :: Bool) (x :: k) (xs :: [k]) :: [k] where
  IfCons False _ xs = xs
  IfCons True x xs = x : xs

type instance Cmp (k :: Symbol) (k' :: Symbol) = CmpSymbol k k'
