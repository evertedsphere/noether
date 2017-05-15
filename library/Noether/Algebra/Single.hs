{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Noether.Algebra.Single where

import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Single.Synonyms
import           Noether.Algebra.Single.Types

cancel :: forall op a. Cancellative op a => Proxy op -> a -> a
cancel p = cancelK p (Proxy :: Proxy (CancellativeS op a))

binaryOp :: forall op a. Magma op a => Proxy op -> a -> a -> a
binaryOp p = binaryOpK p (Proxy :: Proxy (MagmaS op a))

neutral :: forall op a. Neutral op a => Proxy op -> a
neutral p = neutralK p (Proxy :: Proxy (NeutralS op a))

zero :: Neutral Add a => a
zero = neutral AddP

one :: Neutral Mul a => a
one = neutral MulP

negate :: Cancellative Add a => a -> a
negate = cancel AddP

reciprocal :: Cancellative Mul a => a -> a
reciprocal = cancel MulP

infixl 6 +

(+) :: Magma Add a => a -> a -> a
(+) = binaryOp AddP

infixl 7 *

(*) :: Magma Mul a => a -> a -> a
(*) = binaryOp MulP

infixl 6 -

(-) :: (Cancellative Add a, Magma Add a) => a -> a -> a
a - b = a + negate b

infixl 7 /

(/) :: (Cancellative Mul a, Magma Mul a) => a -> a -> a
a / b = a * reciprocal b

