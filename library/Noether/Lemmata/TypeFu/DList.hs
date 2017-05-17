module Noether.Lemmata.TypeFu.DList
  ( The
  , type (%-)
  , layer
  , nil
  ) where

import           Noether.Lemmata.TypeFu

data The k (x :: k)

type (%-) = The

data DList (xs :: [Type]) :: Type where
  Nil :: DList '[]
  Cons :: Proxy (The k x) -> DList xs -> DList (The k x : xs)

layer :: forall k (x :: k) xs. DList xs -> DList (k %- x : xs)
layer = Cons Proxy

nil :: DList '[]
nil = Nil

data Foo = Bar | Baz

type A = DList '[Foo %- Bar, Type %- Type, Nat %- 1]

a :: A
a = layer (layer (layer nil))
