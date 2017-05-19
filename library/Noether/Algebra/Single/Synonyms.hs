module Noether.Algebra.Single.Synonyms where

import           Noether.Lemmata.TypeFu (Proxy(..))

import           Noether.Algebra.Tags

-- Some proxy synonyms

pattern AddP :: Proxy Add
pattern AddP = Proxy

pattern MulP :: Proxy Mul
pattern MulP = Proxy

pattern AndP :: Proxy And
pattern AndP = Proxy

pattern OrP :: Proxy Or
pattern OrP = Proxy

pattern XorP :: Proxy Xor
pattern XorP = Proxy
