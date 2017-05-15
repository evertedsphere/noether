{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Noether.Algebra.Single.Derived where

import qualified Prelude                      as P

import           Data.Complex

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Single.Types


-- instance NeutralK o a =>
--          NeutralK o (i -> a) Prim where
--   neutralK proxy _ = neutralK proxy

-- instance MagmaK o a =>
--          MagmaK o (i -> a) Prim where
--   binaryOpK proxy f g = \x -> binaryOpK proxy (f x) (g x)

-- instance CancellativeK o a =>
--          CancellativeK o (i -> a) Prim where
--   cancelK proxy f = \x -> cancelK proxy (f x)

-- instance SemigroupK o a =>
--          SemigroupK o (i -> a)

-- instance CommutativeK o a =>
--          CommutativeK o (i -> a)

-- instance DistributesOver p m a =>
--          DistributesOver p m (i -> a)

-- instance (NeutralK o a, NeutralK o b) =>
--          NeutralK o (a, b) Prim where
--   neutralK = \p -> (neutralK p, neutralK p)

-- instance (MagmaK o a, MagmaK o b) =>
--          MagmaK o (a, b) Prim where
--   binaryOpK proxy (a, b) (a', b') = (binaryOpK proxy a a', binaryOpK proxy b b')

-- instance (CancellativeK o a, CancellativeK o b) =>
--          CancellativeK o (a, b) Prim where
--   cancelK proxy (a, b) = (cancelK proxy a, cancelK proxy b)

-- instance (SemigroupK o a, SemigroupK o b) =>
--          SemigroupK o (a, b)

-- instance (CommutativeK o a, CommutativeK o b) =>
--          CommutativeK o (a, b)

-- instance (DistributesOver p m a, DistributesOver p m b) =>
--          DistributesOver p m (a, b)


-- -- instance Group o a => Group o (i -> a)


-- FIXME:
-- type family Strategy (a :: k) Prim where
--   Strategy CommSemigroup = CommSemigroupS


-- instance (SemigroupK op a s p, CommutativeK op a s q) =>
--          CommSemigroupK op a s p q (DerivedFrom [p, q])

-- instance (SemigroupK op a s, NeutralK op a s)  => Monoid op a s
-- instance (Monoid op a s, CommutativeK op a s) => CommMonoid op a s

-- instance (Monoid op a s, CancellativeK op a s) => Group op a s
-- instance (Group op a s, CommutativeK op a s) => AbelianGroup op a s


-- type instance CommutativeS Add Double = Prim
-- instance CommutativeK Add Double Prim

-- instance SemigroupK Add Double Prim

-- instance DistributesOver Add Mul Double
