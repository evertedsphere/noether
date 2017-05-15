{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Noether.Algebra.Multiple.Types where

import qualified Prelude                      as P

import           Data.Complex

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Single.Types

-- -- Two binary operations

-- -- | Semirings, aka "rigs"
-- class ( CommMonoid add a s
--       , Monoid mul a s
--       , DistributesOver add mul a s
--       ) => Semiring add mul a s
-- class (Semiring add mul a s, CommutativeK mul a s) => CommSemiring add mul a s

-- class ( AbelianGroup add a s
  --       , Monoid mul a s
--       , DistributesOver add mul a s
--       ) => Ring add mul a s
-- class (Ring add mul a s, CommutativeK mul a s) => CommRing add mul a s

-- class (Ring add mul a s, CancellativeK mul a s) => DivisionRing add mul a s
-- class (Ring add mul a s, AbelianGroup mul a s) => Field add mul a s

-- -- Convenience synonyms

-- type Semiring'            = Semiring     Add Mul
-- type CommSemiring'        = CommSemiring Add Mul
-- type Ring'                = Ring         Add Mul
-- type CommRing'            = CommRing     Add Mul
-- type DivisionRing'        = DivisionRing Add Mul
-- type Field'               = Field        Add Mul

-- -- | Instances

-- data Composite s t
-- instance ( CommMonoid add a s
--          , Monoid mul a s
--          , DistributesOver add mul a s
--          ) => Semiring add mul a s

-- instance (Semiring add mul a s, CommutativeK mul a s) => CommSemiring add mul a s

-- instance ( AbelianGroup add a s
--          , Monoid mul a s
--          , DistributesOver add mul a s
--          ) => Ring add mul a s

-- instance (Ring add mul a s, CommutativeK mul a s) => CommRing add mul a s

-- instance (Ring add mul a s, CancellativeK mul a s) => DivisionRing add mul a s
-- instance (Ring add mul a s, AbelianGroup mul a s) => Field add mul a s
