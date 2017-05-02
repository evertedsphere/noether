{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Unsafe            #-}

module Lemmata.Unsafe (
  unsafeHead,
  unsafeTail,
  unsafeInit,
  unsafeLast,
  unsafeFromJust,
  unsafeIndex,
  unsafeThrow,
) where

import qualified Control.Exception as Exc
import qualified Data.List         as List
import qualified Data.Maybe        as Maybe
import           Lemmata.Base      (Int)

unsafeHead :: [a] -> a
unsafeHead = List.head

unsafeTail :: [a] -> [a]
unsafeTail = List.tail

unsafeInit :: [a] -> [a]
unsafeInit = List.init

unsafeLast :: [a] -> a
unsafeLast = List.last

unsafeFromJust :: Maybe.Maybe a -> a
unsafeFromJust = Maybe.fromJust

unsafeIndex :: [a] -> Int -> a
unsafeIndex = (List.!!)

unsafeThrow :: Exc.Exception e => e -> a
unsafeThrow = Exc.throw
