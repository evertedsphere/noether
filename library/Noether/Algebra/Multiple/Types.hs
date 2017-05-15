{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Noether.Algebra.Multiple.Types where

import qualified Prelude                      as P

import           Data.Complex

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Single.Types

-- Semirings, aka "rigs"

type family SemiringS (add :: k) (mul :: k) (a :: Type) = (r :: Type)

-- FIXME: add DistributesOver add mul a s
class ( CommMonoid add a
      , Monoid mul a
      ) => SemiringK add mul a s

type Semiring add mul a = SemiringK add mul a (SemiringS add mul a)

-- Semirings, aka "rigs"

type family CommSemiringS (add :: k) (mul :: k) (a :: Type) = (r :: Type)

class ( Semiring add mul a
      , Commutative mul a
      ) => CommSemiringK add mul a s

type CommSemiring add mul a = CommSemiringK add mul a (CommSemiringS add mul a)

-- Rings

type family RingS (add :: k) (mul :: k) (a :: Type) = (r :: Type)

class ( Abelian add a
      , Monoid mul a
      ) => RingK add mul a s

type Ring add mul a = RingK add mul a (RingS add mul a)

type family CommRingS (add :: k) (mul :: k) (a :: Type) = (r :: Type)

class ( Ring add mul a
      , Commutative mul a
      ) => CommRingK add mul a s

type CommRing add mul a = CommRingK add mul a (CommRingS add mul a)

type family DivisionRingS (add :: k) (mul :: k) (a :: Type) = (r :: Type)

class ( Ring add mul a
      , Cancellative mul a
      ) => DivisionRingK add mul a s

type DivisionRing add mul a = DivisionRingK add mul a (DivisionRingS add mul a)

type family FieldS (add :: k) (mul :: k) (a :: Type) = (r :: Type)

class ( Ring add mul a
      , Abelian mul a
      ) => FieldK add mul a s

type Field add mul a = FieldK add mul a (FieldS add mul a)

-- Convenience synonyms

type Semiring'            a = Semiring     Add Mul a
type CommSemiring'        a = CommSemiring Add Mul a
type Ring'                a = Ring         Add Mul a
type CommRing'            a = CommRing     Add Mul a
type DivisionRing'        a = DivisionRing Add Mul a
type Field'               a = Field        Add Mul a
