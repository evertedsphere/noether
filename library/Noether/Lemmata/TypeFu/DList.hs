{-# LANGUAGE FunctionalDependencies #-}
module Noether.Lemmata.TypeFu.DList where

import           Noether.Lemmata.TypeFu
import           Noether.Lemmata.TypeFu.Set

data The k (x :: k) = The

data Tagged (s :: Symbol) k (x :: k) = Tagged

type instance Cmp (Tagged s1 _ _) (Tagged s2 _ _) = Cmp s1 s2

data DList (xs :: [Type]) :: Type where
  Nil :: DList '[]
  Tag :: The Symbol s -> The k x -> DList xs -> DList (Tagged s k x : xs)

nil :: DList '[]
nil = Nil

data Foo = Bar | Baz

type (**) k (x :: k) = ('The :: The k x)

infixr 5 ~>

type (~>) (s :: Symbol) (the :: The k x) = Tag ('The :: The Symbol s) the

infixr 0 |-|
type (|-|) a b = a b

infixr 0 |<|
type (|<|) a b = a (b Nil)

infixr 0 |>|
type (|>|) a b = Conv (a b)

type family Conv (a :: DList xs) where
  Conv (_ :: DList xs) = Sort xs

newtype DList_ xs = DList_ (Proxy (Sort xs))

type B
   =  "index"    ~> Nat    ** 1
  |-| "type"     ~> Type   ** Type
  |-| "afoo"     ~> Nat    ** 2
  |-| "akoe"     ~> Nat    ** 2
  |-| "pqr"      ~> Type   ** (Type -> Type)
  |<| "aardvark" ~> [Type] ** '[Type]

type A
   =  "index"    ~> Nat    ** 1
  |>| "type"     ~> Type   ** Type
  |-| "afoo"     ~> Nat    ** 2
  |-| "akoe"     ~> Nat    ** 2
  |-| "pqr"      ~> Type   ** (Type -> Type)
  |<| "aardvark" ~> [Type] **
       (  "b"  ~> Type ** Nat
      |>| "aa" ~> Type ** (Type, Type)
      |<| "a"  ~> Nat  ** 1)

class A__ (a :: [Type]) (b :: Type) | a -> b

data ATag
data BTag

class A_ (a :: [Type])

instance (a ~ A) => A_ a
instance A_ a => A__ a ATag

f :: A__ A ATag => Proxy Nat
f = Proxy

a :: Proxy A
a = Proxy
