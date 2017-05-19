module Noether.Algebra.Single.Cancellative where

import qualified Prelude                 as P

import           Noether.Algebra.Tags
import           Noether.Lemmata.Prelude hiding (Num)
import           Noether.Lemmata.TypeFu

data CancellativeE
  = CancellativeNum
  | CancellativeFractional
  | CancellativeNamed Symbol CancellativeE

class CancellativeK (op :: k) a (s :: CancellativeE) where
  cancelK :: Proxy op -> Proxy s -> a -> a

instance P.Num a => CancellativeK Add a CancellativeNum where
  cancelK _ _ = (0 P.-)

instance P.Fractional a => CancellativeK Mul a CancellativeFractional where
  cancelK _ _ = (1 P./)

instance (KnownSymbol sym, CancellativeK op a s) =>
         CancellativeK op a (CancellativeNamed sym s) where
  cancelK opP _ = cancelK opP (Proxy :: Proxy s)

type Cancellative op a = (CancellativeK $> CancellativeS) op a

type family CancellativeS (op :: k) (a :: Type) = (r :: CancellativeE)
