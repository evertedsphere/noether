module Noether.Algebra.Single.Monoid where

import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Single.Neutral
import           Noether.Algebra.Single.Semigroup

data MonoidE
  = Monoid_Semigroup_Neutral SemigroupE NeutralE
  | MonoidNamed Symbol MonoidE

class MonoidK (op :: k) a (s :: MonoidE)

instance (SemigroupK op a zs, NeutralK op a zn) =>
         MonoidK op a (Monoid_Semigroup_Neutral zs zn)

instance (KnownSymbol sym, MonoidK op a s) =>
         MonoidK op a (MonoidNamed sym s)

type MonoidC op a = (MonoidK $> MonoidS) op a
type Monoid op a = (MonoidC &. Semigroup) op a

type family MonoidS (op :: k) (a :: Type) = (r :: MonoidE)
