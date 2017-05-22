{-# LANGUAGE NoMonomorphismRestriction #-}
module Tagless where

import           Noether.Lemmata.Prelude
import           Prelude

class ExpSym r where
  lit :: Int -> r
  neg :: r -> r
  add :: r -> r -> r

instance ExpSym Int where
  lit = id
  neg x = -x
  add = (+)

instance ExpSym String where
  lit = show
  neg x = "-" <> x
  add x y = x <> " + " <> y

class MulSym r where
  mul :: r -> r -> r

instance MulSym Int where
  mul = (*)

instance MulSym String where
  mul x y = x <> " * " <> y

f = add (neg (lit 4)) (lit 3 `add` (lit 3 `mul` lit 4))
