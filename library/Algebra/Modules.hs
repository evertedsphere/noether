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
-- import           Data.Kind  (Type)
import           Data.Proxy
-- import           GHC.TypeLits (Symbol)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Prelude                   hiding (Monoid, negate, recip, (*),
                                            (+), (-), (/))
import qualified Prelude                   as P

-- | A left module over the ring (add, mul, r) with action tagged by ltag.
class ( Ring add mul r
      , AbelianGroup vadd v
      , AssocLeftActs add r vadd v mul
      , LeftLinear add r vadd v mul
      , AssocLeftActs mul r vadd v mul
      , LeftCompatible mul r vadd v mul
      ) => LeftModule add mul r vadd v

type LeftModule' r v = LeftModule Add Mul r Add v

class ( Ring add mul r
      , AbelianGroup vadd v
      , AssocRightActs add r vadd v mul
      , RightLinear add r vadd v mul
      , AssocRightActs mul r vadd v mul
      , RightCompatible mul r vadd v mul
      ) => RightModule add mul r vadd v

type RightModule' r v = RightModule Add Mul r Add v

-- | R,S-bimodules.

class ( LeftModule radd rmul r vadd v
      , RightModule sadd smul s vadd v
      ) => Bimodule radd rmul r sadd smul s vadd v

type Bimodule' r s = Bimodule Add Mul r Add Mul s Add

type Bimodule_ r = Bimodule' r r

class ( Bimodule add mul r add mul r vadd v
      , Field add mul r
      ) => VectorSpace add mul r vadd v

instance ( Bimodule add mul r add mul r vadd v
         , Field add mul r
         ) => VectorSpace add mul r vadd v

type VectorSpace' r v = VectorSpace Add Mul r Add v

-- -- | Default instances

-- -- | Every magma both left and right-acts on itself.
-- -- | (Not that anyone cares.)

-- instance Magma op a =>
--          LeftActs' op a a where
--   leftAct' _ = binaryOp (undefined :: Proxy op)

-- -- instance {-# OVERLAPPABLE #-} Magma op a =>
-- --          RightActs' op a a where
-- --   rightAct' _ = binaryOp (undefined :: Proxy op)

-- -- | A left semigroup action can be turned into a right action
-- -- if the semigroup is commutative.

-- -- instance {-# OVERLAPPABLE #-}
-- --   (Commutative op a, AssocLeftActs op a tag b) =>
-- --          AssocRightActs op a tag b where
-- --   rightAct = flip leftAct'

-- -- instance {-# OVERLAPPING #-} (Semigroup op a, LeftActs' tag a b) =>
-- --          AssocLeftActs op a tag b where
-- --   leftAct p _ = leftAct' p

-- -- instance {-# OVERLAPPING #-} (Semigroup op a, RightActs' tag a b) =>
-- --          AssocRightActs op a tag b where
-- --   rightAct p _ = rightAct' p

-- -- | Abelian groups are Z-(bi)modules.

-- instance {-# OVERLAPPABLE #-} (AbelianGroup op a) =>
--          AssocLeftActs Mul Integer op a Mul where
--   leftAct _ _ n' a =
--     let
--       mulProxy = Proxy :: Proxy Mul
--       opProxy = Proxy :: Proxy op
--     in case n' of
--          0 -> neutral opProxy
--          1 -> a
--          n -> binaryOp opProxy a (leftAct opProxy mulProxy (n P.- 1) a) {- FIXME slow! -}

-- -- | (The additive group of) every ring is both a left and a right module over itself.

-- -- | {_R}R
-- instance ( Ring add mul r
--          , add' ~ add
--          ) =>
--          LeftModule add mul r add' r

-- -- | {_R}R
-- instance ( Ring add mul r
--          , add' ~ add
--          ) =>
--          RightModule add mul r add' r

leftAnnihilates
  :: ( Eq a
     , Monoid Add a
     , LeftModule' r a
     ) => r -> a -> Bool
leftAnnihilates r a = r <# a == zero

(<#)
  :: AssocLeftActs Add r Add v Mul
  => r -> v -> v
r <# v = leftAct pa pa pm r v
  where
    pa = Proxy :: Proxy Add
    pm = Proxy :: Proxy Mul

-- -- (#>)
-- --   :: AssocRightActs Add r Add v Mul
-- --   => r -> v -> v
-- -- r #> v = rightAct p p v r
-- --   where p = Proxy :: Proxy Add

-- -- (<!)
-- --   :: LeftActs' Mul r r
-- --   => r -> r -> r
-- -- r <! v = leftAct' (undefined :: Proxy Mul) r v

-- -- invertedScale
-- --   :: (VectorSpace' r v)
-- --   => r -> v -> v
-- -- invertedScale r v = reciprocal r <# v

-- -- lerp :: (VectorSpace' r v)
-- --   => r -> v -> v -> v
-- -- lerp by v w = by <# v + (one - by) <# w

-- -- -- | A left module over a commutative ring is "equivalent" to a right module.
-- -- instance
-- --   ( CommutativeRing add mul r
-- --   , CommutativeSemigroup op r -- FIXME why?
-- --   , LeftModule op add mul r op a
-- --   , add ~ Add
-- --   , mul ~ Mul
-- --   ) =>
-- --   RightModule op add mul r op a

-- -- -- | By associativity, those two actions are compatible:

-- -- instance ( Ring radd rmul r
-- --          , radd ~ Add
-- --          , rmul ~ Mul
-- --          , sadd ~ Add
-- --          , smul ~ Mul
-- --          , r ~ s
-- --          , r ~ t
-- --          , s ~ t -- is this needed?
-- --          , vtag ~ Add
-- --          , rmul' ~ rmul
-- --          , smul' ~ smul
-- --          ) =>
-- --          Bimodule rmul' radd rmul r smul' sadd smul s vtag t

-- -- -- | Abelian groups are Z-(bi)modules.
-- -- instance ( AbelianGroup op a
-- --          , add ~ Add
-- --          , mul ~ Mul
-- --          , op' ~ op
-- --          ) =>
-- --          LeftModule op' add mul Integer op a
