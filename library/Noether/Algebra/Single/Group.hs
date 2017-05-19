module Noether.Algebra.Single.Group where

import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Single.Cancellative
import           Noether.Algebra.Single.Commutative
import           Noether.Algebra.Single.Monoid

type family GroupS (op :: k) (a :: Type) = (r :: GroupE)

data GroupE
  = MonoidCancellative MonoidE CancellativeE
  | GroupNamed Symbol GroupE

class GroupK (op :: k) a (s :: GroupE)

instance (MonoidK op a zm, CancellativeK op a zl) =>
         GroupK op a (MonoidCancellative zm zl)

instance (KnownSymbol sym, GroupK op a s) =>
         GroupK op a (GroupNamed sym s)

type GroupC op a = (GroupK $> GroupS) op a
type Group op a = (GroupC &. Monoid &. Cancellative) op a
