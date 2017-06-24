module Noether.Lemmata.Prelude
  ( module X
  , fromInteger
  , fromString
  ) where

import           Data.Complex as X
import           Data.Monoid  as X ((<>))
import           Prelude      as X hiding (Eq, Monoid, fromInteger, negate,
                                    recip, (&&), (*), (+), (-), (/), (==), (||))

import           Data.Ratio   as X
import           Data.Int     as X
import qualified Data.String  as S
import qualified Prelude      as P

fromInteger :: Num a => Integer -> a
fromInteger = P.fromInteger

fromString :: S.IsString a => String -> a
fromString = S.fromString

