{-# LANGUAGE AllowAmbiguousTypes #-}
module Noether.Algebra.Single.Synonyms where

import           Noether.Algebra.Single.Types
import           Noether.Lemmata.TypeFu

-- Some proxy synonyms

neutralSP :: Proxy (NeutralS op a)
neutralSP = Proxy

cancellativeSP :: Proxy (CancellativeS op a)
cancellativeSP = Proxy

magmaSP :: Proxy (MagmaS op a)
magmaSP = Proxy
