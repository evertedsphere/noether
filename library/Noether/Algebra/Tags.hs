module Noether.Algebra.Tags
  ( BinaryNumeric(..)
  , BinaryBoolean(..)
  , Side(..)
  ) where

data BinaryNumeric = Add | Mul
data BinaryBoolean = And | Or | Xor

-- | Oh, Either...
data Side = L | R

