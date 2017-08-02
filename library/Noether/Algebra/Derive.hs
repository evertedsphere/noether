{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language UndecidableSuperClasses #-}

module Noether.Algebra.Derive where

import Data.Profunctor
import Data.Profunctor.Traversing
import Noether.Lemmata.TypeFu
import Noether.Lemmata.Prelude

type Idempotent f a = f (f a) ~ f a

class
  ( Idempotent Unindexed p
  , Indexable i (Unindexed p)
  ) => Indexable i p where
    type Unindexed p :: Type -> Type -> Type
    indexed :: p a b -> i -> Unindexed p a b

instance Indexable i (->) where
  type Unindexed (->) = (->)
  indexed x _ = x

newtype Indexed i a b = Indexed { runIndexed :: i -> a -> b }

instance (i ~ j) => Indexable i (Indexed j) where
  type Unindexed (Indexed j) = (->)
  indexed = runIndexed

type IndexedTraversal i s t a b = forall p. ( Indexable i p
                                            , Traversing p
                                            , Traversing (Unindexed p)
                                            ) =>
                                              p a b -> Unindexed p s t

foo :: IndexedTraversal Int a b [a] [b]
foo = foo

bar :: IndexedTraversal Int a b [[[a]]] [[[b]]]
bar = foo.foo.foo

