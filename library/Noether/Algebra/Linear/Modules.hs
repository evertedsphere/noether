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

module Noether.Algebra.Linear.Modules where

import           Prelude                        hiding (Monoid, fromInteger,
                                                 negate, recip, (*), (+), (-),
                                                 (/))

import           Data.Complex

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Actions
import           Noether.Algebra.Multiple.Types
import           Noether.Algebra.Single
import           Noether.Algebra.Single.Types

-- | A left module (v, a) over the ring (r, p, m).
class ( Ring p m r
      , Abelian a v
      , LeftLinear p r a v
      , LeftCompatible m r v
      ) => LeftModuleK p m r a v s

type family LeftModuleS (p :: k1) (m :: k2) r (a :: k3) v :: k

type LeftModule p m r a v = LeftModuleK p m r a v (LeftModuleS p m r a v)

-- | A right module (v, a) over the ring (r, p, m).
class ( Ring p m r
      , Abelian a v
      , RightLinear p r a v
      , RightCompatible m r v
      ) => RightModuleK p m r a v s

type family RightModuleS (p :: k1) (m :: k2) r (a :: k3) v :: k

type RightModule p m r a v = RightModuleK p m r a v (RightModuleS p m r a v)

-- | R,S-bimodules.
class ( LeftModule p m r a v
      , RightModule q n s a v
      ) => BimoduleK p m r q n s a v z

type family BimoduleS (p :: k1) (m :: k2) r (q :: k3) (n :: k4) s (a :: k5) v :: k
type Bimodule p m r q n s a v = BimoduleK p m r q n s a v (BimoduleS p m r q n s a v)

-- | An R,R-bimodule for commutative R is just called an R-module.

type Module p m r a v = (Bimodule p m r p m r a v, CommRing p m r)

-- | A vector space over the field (k,p,m).

class ( Module p m k a v
      , Field p m k
      ) => VectorSpaceK p m k a v z

type family VectorSpaceS (p :: k1) (m :: k2) k (a :: k3) v :: k4

type VectorSpace p m k a v = VectorSpaceK p m k a v (VectorSpaceS p m k a v)

data InnerProductTag = DotProduct

-- pattern DotProductP :: Proxy DotProduct
-- pattern DotProductP = Proxy

class (VectorSpace p m k a v) =>
      InnerProductSpaceK ip p m k a v s where
  innerProductK :: Proxy ip -> Proxy p -> Proxy m -> Proxy a -> Proxy s -> v -> v -> k

type family InnerProductSpaceS (ip :: k0) (p :: k1) (m :: k2) k (a :: k3) v :: k4

type InnerProductSpace ip p m k a v = InnerProductSpaceK ip p m k a v (InnerProductSpaceS ip p m k a v)

----------------------------------------------------------------------------------
--- Convenience synonyms
----------------------------------------------------------------------------------

type RightModule'          r v = RightModule Add Mul r Add v
type LeftModule'           r v = LeftModule  Add Mul r Add v

type Bimodule'           r s v = Bimodule  Add Mul r Add Mul s Add v
type Bimodule_             r v = Bimodule  Add Mul r Add Mul r Add v
type Module'               r v = Module    Add Mul r Add v

type VectorSpace'          k v = VectorSpace           Add Mul k Add v
type InnerProductSpace' ip k v = InnerProductSpace  ip Add Mul k Add v
type DotProductSpace'      k v = InnerProductSpace' DotProduct k v

-- ----------------------------------------------------------------------------------
-- --- "Derived" instances. Possibly too general?
-- ----------------------------------------------------------------------------------

-- -- instance Module p m r a v => Module p m r a (i -> v)

-- instance ( Bimodule p m r p m r a v
--          , CommRing p m r
--          ) => Module p m r a v

-- instance ( Module p m r a v
--          , Field p m r
--          ) => VectorSpace p m r a v

-- ----------------------------------------------------------------
-- -- Rings are modules
-- ---
-- -- (The additive group of) every ring is both a left and a right
-- -- module over itself.
-- ----------------------------------------------------------------

-- instance (Ring p m r, LeftLinear p r p r, LeftCompatible m r r) =>
--          LeftModuleK p m r p r Known

-- -- | (R,p,m) is a module in the category Mod-R.
-- instance ( Ring p m r
--          , p ~ Add
--          , m ~ Mul
--          ) => RightModule p m r Add r

-- -- | By associativity, the actions are commutative, giving (R, p) an
-- -- | R,R-bimodule (aka "R-bimodule") structure.
-- instance ( Ring p m r
--          , p ~ Add
--          , m ~ Mul
--          , q ~ p
--          , n ~ m
--          ) => Bimodule p m r q n r Add r

-- --------------------------------------------------------------------
-- -- C is a vector space over R
-- --------------------------------------------------------------------

-- instance LeftModule'  Double (Complex Double)
-- instance RightModule' Double (Complex Double)
-- instance Bimodule_    Double (Complex Double)

-- --------------------------------------------------------------------
-- -- Vector spaces
-- --------------------------------------------------------------------

-- -- | Every field is an inner product space over itself.
-- instance ( Field p m k
--          , p ~ Add
--          , m ~ Mul
--          ) => InnerProductSpace DotProduct p m k Add k where
--   innerProductP _ _ _ _ = (*)

-- -- | 2-vectors
-- instance ( LeftModule' r a
--          , LeftModule' r b
--          , p ~ Add
--          , m ~ Mul
--          ) => LeftModule p m r Add (a, b)

-- instance ( RightModule' r a
--          , RightModule' r b
--          , p ~ Add
--          , m ~ Mul
--          ) => RightModule p m r Add (a, b)

-- instance {-# INCOHERENT #-}
--          ( Bimodule_ r a
--          , Bimodule_ r b
--          , p ~ Add
--          , m ~ Mul
--          , q ~ p
--          , n ~ m
--          ) => Bimodule p m r q n r Add (a, b)

-- instance {-# INCOHERENT #-}
--          ( DotProductSpace' k v
--          , DotProductSpace' k w
--          , p ~ Add
--          , m ~ Mul
--          ) => InnerProductSpace DotProduct p m k Add (v, w) where
--   innerProductP p1 p2 p3 dp (a,b) (c,d) =
--     innerProductP p1 p2 p3 dp a c + innerProductP p1 p2 p3 dp b d

-- leftAnnihilates
--   :: ( Eq a
--      , Monoid Add a
--      , LeftModule' r a
--      ) => r -> a -> Bool
-- leftAnnihilates r a = r %< a == zero

-- actionCommutes
--   :: ( Eq a
--      , Monoid Add a
--      , Bimodule_ r a
--      ) => r -> a -> Bool
-- actionCommutes r a = r %< a == a >% r

-- (%<) :: LeftModule' r v => r -> v -> v
-- r %< v = leftAct AddP AddP MulP r v

-- (>%) :: RightModule' r v => v -> r -> v
-- v >% r = rightAct AddP AddP MulP r v

-- invertedScale :: VectorSpace' r v => r -> v -> v
-- invertedScale r v = reciprocal r %< v

-- -- | Linear interpolation.
-- -- lerp λ v w = λv + (1 - λ)w
-- lerp
--   :: VectorSpace' r v
--   => r -> v -> v -> v
-- lerp lambda v w = lambda %< v + w >% (one - lambda)

-- lol :: (Complex Double, Complex Double)
-- lol =
--   (1, 3) * lerp lambda (3, 3) (4, 5) + (1, 0) >% lambda + v + lambda %< w +
--   (lambda, -lambda)

--   where
--     lambda :: Complex Double
--     lambda = 0.3 :+ 1

--     v = (3, 3)
--     w = (2, 7)

-- lol' :: (Double, Double)
-- lol' = lerp @Double 0.3 (3, 3) (4, 5)

-- dot :: DotProductSpace' r v => v -> v -> r
-- dot = innerProductP AddP MulP AddP DotProductP

