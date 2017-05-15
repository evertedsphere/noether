{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Noether.Algebra.Single where

import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Single.Synonyms
import           Noether.Algebra.Single.Types

cancel :: forall op a . Cancellative op a => Proxy op -> a -> a
cancel p = cancelK p (cancellativeSP @op @a)

binaryOp :: forall op a . Magma op a => Proxy op -> a -> a -> a
binaryOp p = binaryOpK p (magmaSP @op @a)

neutral :: forall op a. Neutral op a => Proxy op -> a
neutral p = neutralK p (neutralSP @op @a)

infixl 6 +

(+) :: Semigroup Add a => a -> a -> a
(+) = binaryOp AddP

infixl 7 *

(*) :: Semigroup Mul a => a -> a -> a
(*) = binaryOp MulP

zero :: Neutral Add a => a
zero = neutral AddP

one :: Neutral Mul a => a
one = neutral MulP

negate :: Abelian Add a => a -> a
negate = cancel AddP

reciprocal :: Group Mul a => a -> a
reciprocal = cancel MulP

infixl 6 -

(-) :: Abelian Add a => a -> a -> a
a - b = a + negate b

infixl 7 /

(/) :: Group Mul a => a -> a -> a
a / b = a * reciprocal b

