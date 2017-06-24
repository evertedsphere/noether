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

cancel :: forall op a. Cancellative op a => Proxy op -> a -> a
cancel p = cancelK p (Proxy @(CancellativeS op a))

binaryOp :: forall op a. Magma op a => Proxy op -> a -> a -> a
binaryOp p = binaryOpK p (Proxy @(MagmaS op a))

neutral :: forall op a. Neutral op a => Proxy op -> a
neutral p = neutralK p (Proxy @(NeutralS op a))

zero :: Neutral Add a => a
zero = neutral AddP

one :: Neutral Mul a => a
one = neutral MulP

true :: Neutral And a => a
true = neutral AndP

false :: Neutral Or a => a
false = neutral OrP

negate :: Cancellative Add a => a -> a
negate = cancel AddP

reciprocal :: Cancellative Mul a => a -> a
reciprocal = cancel MulP

-- Addition, multiplication

infixl 6 +

(+) :: Magma Add a => a -> a -> a
(+) = binaryOp AddP

infixl 7 *

(*) :: Magma Mul a => a -> a -> a
(*) = binaryOp MulP

-- Groupy things

infixl 6 -

(-) :: (Magma Add a, Cancellative Add a) => a -> a -> a
a - b = a + negate b

infixl 7 /

(/) :: (Magma Mul a, Cancellative Mul a) => a -> a -> a
a / b = a * reciprocal b

-- Binary operations

infixl 3 &&

(&&) :: Magma And a => a -> a -> a
(&&) = binaryOp AndP

infixl 2 ||

(||) :: Magma Or a => a -> a -> a
(||) = binaryOp OrP
