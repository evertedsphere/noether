{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}

module Noether.Algebra.Single.API where

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu
import           Prelude                             ((==))

import           Noether.Algebra.Single.Cancellative
import           Noether.Algebra.Single.Group
import           Noether.Algebra.Single.Magma
import           Noether.Algebra.Single.Monoid
import           Noether.Algebra.Single.Neutral
import           Noether.Algebra.Single.Semigroup

import           Noether.Algebra.Single.Strategies

import           Noether.Algebra.Single.Synonyms
import           Noether.Algebra.Tags

binaryOp :: forall op a. Magma op a => Proxy op -> a -> a -> a
binaryOp p = binaryOpK p (Proxy @(MagmaS op a))

cancel :: forall op a. Cancellative op a => Proxy op -> a -> a
cancel p = cancelK p (Proxy @(CancellativeS op a))

neutral :: forall op a. Neutral op a => Proxy op -> a
neutral p = neutralK p (Proxy @(NeutralS op a))

-- | A polymorphic additive identity
zero :: Neutral Add a => a
zero = neutral AddP

-- | A polymorphic multiplicative identity
one :: Neutral Mul a => a
one = neutral MulP

-- | A polymorphic conjunctive identity
true :: Neutral And a => a
true = neutral AndP

-- | A polymorphic disjunctive identity
false :: Neutral Or a => a
false = neutral OrP

-- | Additive inverses
negate :: Cancellative Add a => a -> a
negate = cancel AddP

-- | Multiplicative inverses
reciprocal :: Cancellative Mul a => a -> a
reciprocal = cancel MulP

infixl 6 +

-- | Polymorphic addition for magmas
(+) :: Magma Add a => a -> a -> a
(+) = binaryOp AddP

infixl 7 *

-- | Polymorphic multiplication for magmas
(*) :: Magma Mul a => a -> a -> a
(*) = binaryOp MulP


infixl 6 -

-- | Subtraction for cancellative additive magmas
(-) :: (Magma Add a, Cancellative Add a) => a -> a -> a
a - b = a + negate b

infixl 7 /

-- | Division for cancellative multiplicative magmas
(/) :: (Magma Mul a, Cancellative Mul a) => a -> a -> a
a / b = a * reciprocal b

-- Binary operations

infixl 3 &&

-- | Polymorphic conjunction
(&&) :: Magma And a => a -> a -> a
(&&) = binaryOp AndP

infixl 2 ||

-- | Polymorphic disjunction
(||) :: Magma Or a => a -> a -> a
(||) = binaryOp OrP
