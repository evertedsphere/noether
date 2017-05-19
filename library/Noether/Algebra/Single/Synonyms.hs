{-# LANGUAGE AllowAmbiguousTypes #-}
module Noether.Algebra.Single.Synonyms where

import           Noether.Lemmata.TypeFu
import           Noether.Algebra.Tags

-- Some proxy synonyms

pattern AddP :: Proxy Add
pattern AddP = Proxy

pattern MulP :: Proxy Mul
pattern MulP = Proxy

-- neutralSP :: Proxy (NeutralS op a)
-- neutralSP = Proxy

-- cancellativeSP :: Proxy (CancellativeS op a)
-- cancellativeSP = Proxy

-- magmaSP :: Proxy (MagmaS op a)
-- magmaSP = Proxy
