module Noether.Algebra.Single.AbelianGroup where

import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Single.Commutative
import           Noether.Algebra.Single.Group

type family AbelianGroupS (op :: k) (a :: Type) = (r :: AbelianGroupE)

data AbelianGroupE
  = GroupCommutative GroupE CommutativeE
  | AbelianGroupNamed Symbol AbelianGroupE

class AbelianGroupK (op :: k) a (s :: AbelianGroupE)

instance (GroupK op a zm, CommutativeK op a zl) =>
         AbelianGroupK op a (GroupCommutative zm zl)

instance (KnownSymbol sym, AbelianGroupK op a s) =>
         AbelianGroupK op a (AbelianGroupNamed sym s)

type AbelianGroupC op a = (AbelianGroupK $> AbelianGroupS) op a
type AbelianGroup op a = (AbelianGroupC &. Group &. Commutative) op a