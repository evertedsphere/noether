module Noether.Algebra.Single.Group where

import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Single.Cancellative
import           Noether.Algebra.Single.Commutative
import           Noether.Algebra.Single.Magma
import           Noether.Algebra.Single.Monoid

type family GroupS (op :: k) (a :: Type) = (r :: GroupE)

data GroupE
  = Group_Monoid_Cancellative MonoidE CancellativeE
  | GroupNamed Symbol GroupE

class GroupK (op :: k) a (s :: GroupE)

instance (MonoidK op a zm, CancellativeK op a zl) =>
         GroupK op a (Group_Monoid_Cancellative zm zl)

instance (KnownSymbol sym, GroupK op a s) =>
         GroupK op a (GroupNamed sym s)

type GroupC op a = GroupK op a (GroupS op a)
