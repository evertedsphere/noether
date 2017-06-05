{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs        #-}
module Noether.Algebra.Tags where

import           Data.Monoid
import           Prelude

data BinaryNumeric = Add | Mul
data BinaryBoolean = And | Or | Xor

-- | Oh, Either...
data Side = L | R

data FunctionLift = FunctionLift

