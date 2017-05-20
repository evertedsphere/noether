module Noether.Lemmata.TypeFu
  ( module X
  , type (&)
  , type (&.)
  , type (&&.)
  , type ($>)
  , type ($$>)
  ) where

import           GHC.Exts           as X
import           GHC.Prim           as X
import           GHC.TypeLits       as X

import           Data.Coerce        as X
import           Data.Proxy         as X

import           Data.Kind          as X hiding (type (*))
import           Data.Type.Bool     as X
import           Data.Type.Equality as X

infixl 6 &
type (&) (a :: Constraint) (b :: Constraint) = (a, b)

infixl 6 &.
type (&.)
  (a :: k -> k' -> Constraint)
  (b :: k -> k' -> Constraint)
  (p :: k) (q :: k')
  = (a p q, b p q)

infixl 6 &&.
type (&&.)
  (a :: k1 -> k2 -> k3 -> Constraint)
  (b :: k1 -> k2 -> k3 -> Constraint)
  (p :: k1) (q :: k2) (r :: k3)
  = (a p q r, b p q r)

infixl 7 $>
type ($>)
  (a :: k1 -> k2 -> k3 -> k4)
  (b :: k1 -> k2 -> k3)
  (p :: k1) (q :: k2)
  = a p q (b p q)

infixl 7 $$>
type ($$>)
  (a :: k1 -> k2 -> k3 -> k4 -> k5)
  (b :: k1 -> k2 -> k3 -> k4)
  (p :: k1) (q :: k2) (r :: k3)
  = a p q r (b p q r)
