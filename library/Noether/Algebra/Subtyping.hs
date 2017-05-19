module Noether.Algebra.Subtyping
  ( Subtype
  , embed
  ) where

class Subtype a b where
  embed :: a -> b
