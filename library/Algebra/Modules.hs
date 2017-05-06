{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TemplateHaskellQuotes  #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Algebra.Modules where

import           Algebra.Actions
import           Algebra.Basics
import           Prelude         hiding (Monoid, fromInteger, negate, recip,
                                  (*), (+), (-), (/))

-- import qualified Prelude                   as P
-- import           Data.Kind  (Type)
-- import           Data.Proxy
-- import           GHC.TypeLits (Symbol)
-- import           Language.Haskell.TH
-- import           Language.Haskell.TH.Quote

-- | A left module (v, va) over the ring (r, p, m).

class ( Ring p m r
      , AbelianGroup va v
      , LeftLinear p r va v m
      , LeftCompatible m r va v m
      ) => LeftModule p m r va v

type RightModule' r = RightModule Add Mul r Add

class ( Ring p m r
      , AbelianGroup va v
      , RightLinear p r va v m
      , RightCompatible m r va v m
      ) => RightModule p m r va v

type LeftModule' r = LeftModule Add Mul r Add

-- | R,S-bimodules.

class ( LeftModule ra rm r va v
      , RightModule sa sm s va v
      ) => Bimodule ra rm r sa sm s va v

type Bimodule' r s = Bimodule Add Mul r Add Mul s Add

type Bimodule_ r = Bimodule' r r

-- | An R,R-bimodule for commutative R is just called an R-module.
class ( Bimodule p m r p m r va v
      , CommutativeRing p m r
      ) => Module p m r va v

instance ( Bimodule p m r p m r va v
         , CommutativeRing p m r
         ) => Module p m r va v

type Module' r = Module Add Mul r Add

class FreeModule p m r va v

-- | A vector space over the ring (r,p,m).
class ( Module p m r va v
      , Field p m r
      ) => VectorSpace p m r va v

instance ( Module p m r va v
         , Field p m r
         ) => VectorSpace p m r va v

type VectorSpace' r = VectorSpace Add Mul r Add

-- | (The additive group of) every ring is both a left and a right module over itself.

-- | {_R}R
instance ( Ring p m r
         , p ~ Add
         , m ~ Mul
         ) =>
         LeftModule p m r Add r

-- | {_R}R
instance ( Ring p m r
         , p ~ Add
         , m ~ Mul
         ) =>
         RightModule p m r Add r

instance ( Ring p m r
         , p ~ Add
         , m ~ Mul
         , q ~ Add
         , n ~ Mul
         ) =>
         Bimodule p m r q n r Add r

leftAnnihilates
  :: ( Eq a
     , Monoid Add a
     , Bimodule' r r a
     ) => r -> a -> Bool
leftAnnihilates r a = r <% a == zero

(<%)
  :: LeftActs Add r Add v Mul
  => r -> v -> v
r <% v = leftAct AddP AddP MulP r v

(%>)
  :: RightActs Add r Add v Mul
  => r -> v -> v
r %> v = rightAct AddP AddP MulP v r

invertedScale
  :: VectorSpace' r v
  => r -> v -> v
invertedScale r v = reciprocal r <% v

lerp
  :: VectorSpace' r v
  => r -> v -> v -> v
lerp lambda v w = lambda <% v + (one - lambda) %> w -- for variety

-- data (+>) a b where

data Vector (n :: k) p v where
