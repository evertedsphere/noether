module Noether.Algebra.Single.Semigroup where

import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Single.Magma
import           Noether.Algebra.Tags

data SemigroupE
  = FromMagma MagmaE
  | SemigroupNamed Symbol SemigroupE

class SemigroupK (op :: k) a (s :: SemigroupE)

instance MagmaK op a s => SemigroupK op a (FromMagma s)

instance (KnownSymbol sym, SemigroupK op a s) =>
         SemigroupK op a (SemigroupNamed sym s)

type SemigroupC op a = (SemigroupK $> SemigroupS) op a
type Semigroup op a = (SemigroupC &. Magma) op a

type family SemigroupS (op :: k) (a :: Type) = (r :: SemigroupE)

