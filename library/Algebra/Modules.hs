{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TemplateHaskellQuotes  #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Algebra.Modules where

import           Algebra.Actions
import           Algebra.Basics
import           Data.Proxy
import           Prelude         hiding (Monoid, fromInteger, negate, recip,
                                  (*), (+), (-), (/))

-- import qualified Prelude                   as P
-- import           Data.Kind  (Type)
-- import           Data.Proxy
-- import           GHC.TypeLits (Symbol)
-- import           Language.Haskell.TH
-- import           Language.Haskell.TH.Quote

-- | A left module (v, a) over the ring (r, p, m).
class ( Ring p m r
      , AbelianGroup a v
      , LeftLinear p r a v m
      , LeftCompatible m r a v m
      ) => LeftModule p m r a v

-- | A right module (v, a) over the ring (r, p, m).
class ( Ring p m r
      , AbelianGroup a v
      , RightLinear p r a v m
      , RightCompatible m r a v m
      ) => RightModule p m r a v

-- | R,S-bimodules.
class ( LeftModule p m r a v
      , RightModule q n s a v
      ) => Bimodule p m r q n s a v

-- | An R,R-bimodule for commutative R is just called an R-module.
-- | TODO type synonym?
class ( Bimodule p m r p m r a v
      , CommutativeRing p m r
      ) => Module p m r a v

-- class FreeModule p m r a v

-- | A vector space over the ring (r,p,m).
class ( Module p m k a v
      , Field p m k
      ) => VectorSpace p m k a v

data InnerProductTag = DotProduct

pattern DotProductP :: Proxy DotProduct
pattern DotProductP = Proxy

class (VectorSpace p m k a v) =>
      InnerProductSpace (ip :: InnerProductTag) p m k a v where
  innerProductP :: proxy p -> proxy m -> proxy a -> Proxy ip -> v -> v -> k

----------------------------------------------------------------------------------
--- Convenience synonyms
----------------------------------------------------------------------------------

type RightModule'          r = RightModule Add Mul r Add
type LeftModule'           r = LeftModule  Add Mul r Add

type Bimodule'           r s = Bimodule  Add Mul r Add Mul s Add
type Bimodule_             r = Bimodule' r r
type Module'               r = Module    Add Mul r Add

type VectorSpace'          k = VectorSpace        Add Mul k Add
type InnerProductSpace' ip k = InnerProductSpace  ip Add Mul k Add
type DotProductSpace'      k = InnerProductSpace' DotProduct k

----------------------------------------------------------------------------------
--- "Derived" instances. Possibly too general?
----------------------------------------------------------------------------------

-- instance Module p m r a v => Module p m r a (i -> v)

instance ( Bimodule p m r p m r a v
         , CommutativeRing p m r
         ) => Module p m r a v

instance ( Module p m r a v
         , Field p m r
         ) => VectorSpace p m r a v

-- (The additive group of) every ring is both a left and a right module over itself.

-- | (R,p) is a module in the category R-Mod.
instance ( Ring p m r
         , p ~ Add
         , m ~ Mul
         ) => LeftModule p m r Add r

-- | (R,p) is a module in the category Mod-R.
instance ( Ring p m r
         , p ~ Add
         , m ~ Mul
         ) => RightModule p m r Add r

-- | By associativity, the actions are commutative, giving (R, p) an
-- | R,R-bimodule (aka "R-bimodule") structure.
instance ( Ring p m r
         , p ~ Add
         , m ~ Mul
         , q ~ p
         , n ~ m
         ) => Bimodule p m r q n r Add r

-- | Every field is an inner product space over itself.
instance ( Field p m k
         , p ~ Add
         , m ~ Mul
         ) => InnerProductSpace DotProduct p m k Add k where
  innerProductP _ _ _ _ = (*)

-- | 2-vectors
instance ( Ring p m r
         , p ~ Add
         , m ~ Mul
         ) => LeftModule p m r Add (r, r)

instance ( Ring p m r
         , p ~ Add
         , m ~ Mul
         ) => RightModule p m r Add (r, r)

instance ( Ring p m r
         , p ~ Add
         , m ~ Mul
         , q ~ p
         , n ~ m
         ) => Bimodule p m r q n r Add (r, r)

instance ( Field p m k
         , p ~ Add
         , m ~ Mul
         ) => InnerProductSpace DotProduct p m k Add (k, k) where
  innerProductP _ _ _ _ (a,b) (c,d) = a * c + b * d

leftAnnihilates
  :: ( Eq a
     , Monoid Add a
     , Bimodule_ r a
     ) => r -> a -> Bool
leftAnnihilates r a = r %< a == zero

(%<)
  :: LeftModule' r v
  => r -> v -> v
r %< v = leftAct AddP AddP MulP r v

(>%)
  :: RightModule' r v
  => v -> r -> v
v >% r = rightAct AddP AddP MulP r v

invertedScale
  :: VectorSpace' r v
  => r -> v -> v
invertedScale r v = reciprocal r %< v

-- | Linear interpolation.
-- lerp λ v w = λv + (1 - λ)w
lerp
  :: VectorSpace' r v
  => r -> v -> v -> v
lerp lambda v w = lambda %< v + w >% (one - lambda)

lol :: (Double, Double)
lol = (1, 3) * lerp lambda (3, 3) (4, 5) + (1, 0) >% (dot @Double v w)
  where
    lambda :: Double
    lambda = 0.3

    v, w :: (Double, Double)
    v = (3, 3)
    w = (2, 7)

lol' :: (Double, Double)
lol' = lerp @Double 0.3 (3, 3) (4, 5)

dot :: DotProductSpace' r v => v -> v -> r
dot = innerProductP AddP MulP AddP (Proxy :: Proxy DotProduct)

-- data Vector (n :: k) p v where

data a ~> b where
  Dot :: a ~> b
