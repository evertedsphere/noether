{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TemplateHaskellQuotes  #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Algebra.Actions where

import           Algebra.Basics
-- import           Data.Kind  (Type)
import           Data.Proxy
-- import           GHC.TypeLits (Symbol)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Prelude                   hiding (Monoid, negate, recip, (*),
                                            (+), (-), (/))
import qualified Prelude                   as P

data ActionTag = Self BinaryTag | Scalar BinaryTag

class LeftActs (tag :: ActionTag) a b where
  leftAct' :: Proxy tag -> a -> b -> b

class RightActs (tag :: ActionTag) a b where
  rightAct' :: Proxy tag -> b -> a -> b

class (LeftActs ltag a b, RightActs rtag a b) =>
      Acts ltag rtag a b

-- | Associative/semigroup actions

class (Semigroup op a, LeftActs tag a b) => AssocLeftActs op a tag b where
  leftAct :: Proxy tag -> Proxy op -> a -> b -> b

class (Semigroup op a, RightActs tag a b) => AssocRightActs op a tag b where
  rightAct :: Proxy tag -> Proxy op -> b -> a -> b

-- | FIXME: tons of distributivity implied hereafter in the actions, no?

class (AssocLeftActs op a ltag b, AssocRightActs op a rtag b) =>
      AssocActs op a ltag rtag b

class (Group op a, AssocLeftActs op a ltag b) => GSet op a ltag b

-- | A left module over the ring (add, mul, r) with action tagged by ltag.
class ( Ring add mul r
      , AbelianGroup vadd v
      , AssocLeftActs add r ltag v
      , AssocLeftActs mul r ltag v
      ) =>
      LeftModule ltag add mul r vadd v

type LeftModule' ltag r v = LeftModule ltag Add Mul r Add v

class ( Ring add mul r
      , AbelianGroup vadd v
      , AssocRightActs add r rtag v
      , AssocRightActs mul r rtag v
      ) =>
      RightModule rtag add mul r vadd v

type RightModule' rtag r v = RightModule rtag Add Mul r Add v

-- | R,S-bimodules.

class ( LeftModule rtag radd rmul r vadd v
      , RightModule stag sadd smul s vadd v
      ) =>
      Bimodule rtag radd rmul r stag sadd smul s vadd v

type Bimodule' rtag r stag s = Bimodule rtag Add Mul r stag Add Mul s

type Bimodule_ tag r = Bimodule tag Add Mul r tag Add Mul r

class ( Bimodule tag add mul r tag add mul r vadd v
      , Field add mul r
      ) =>
      VectorSpace tag add mul r vadd v

type VectorSpace' tag r v = VectorSpace tag Add Mul r Add v

-- | Default instances

-- | Every magma both left and right-acts on itself.

instance Magma op a =>
         LeftActs (Self op) a a where
  leftAct' _ = binaryOp (undefined :: Proxy op)

instance {-# OVERLAPPABLE #-} Magma op a =>
         RightActs (Self op) a a where
  rightAct' _ = binaryOp (undefined :: Proxy op)

-- | A left semigroup action can be turned into a right action
-- if the semigroup is commutative.

instance {-# OVERLAPPABLE #-}
  (CommutativeSemigroup op a, LeftActs tag a b, tag ~ Scalar op) =>
         RightActs tag a b where
  rightAct' = flip . leftAct'

instance (Semigroup op a, LeftActs tag a b) =>
         AssocLeftActs op a tag b where
  leftAct p _ = leftAct' p

instance (Semigroup op a, RightActs tag a b) =>
         AssocRightActs op a tag b where
  rightAct p _ = rightAct' p

-- | Abelian groups are Z-(bi)modules.

instance (AbelianGroup op a) =>
         LeftActs (Scalar op) Integer a where
  leftAct' sproxy n' a =
    let proxy = (undefined :: Proxy op)
    in case n' of
      0 -> neutral proxy
      1 -> a
      n -> {- FIXME slow! -} binaryOp proxy a (leftAct' sproxy (n P.- 1) a)

-- | (The additive group of) every ring is both a left and a right module over itself.

-- | {_R}R
instance ( Ring add mul r
         , add ~ Add
         , mul ~ Mul
         , vadd ~ Add
         , mul' ~ mul
         , r ~ s
         ) =>
         LeftModule (Self mul') add mul r vadd s

-- | R_R
instance ( Ring add mul r
         , tag ~ Self mul
         , add ~ Add
         , mul ~ Mul
         , vadd ~ Add
         , mul' ~ mul
         ) =>
         RightModule (Self mul') add mul r vadd r

-- | A left module over a commutative ring is "equivalent" to a right module.
instance
  ( CommutativeRing add mul r
  , CommutativeSemigroup op r -- FIXME why?
  , LeftModule (Scalar op) add mul r op a
  , add ~ Add
  , mul ~ Mul
  ) =>
  RightModule (Scalar op) add mul r op a

-- | By associativity, those two actions are compatible:

instance ( Ring radd rmul r
         , radd ~ Add
         , rmul ~ Mul
         , sadd ~ Add
         , smul ~ Mul
         , r ~ s
         , r ~ t
         , s ~ t -- is this needed?
         , vtag ~ Add
         , rmul' ~ rmul
         , smul' ~ smul
         ) =>
         Bimodule (Self rmul') radd rmul r (Self smul') sadd smul s vtag t

-- | Abelian groups are Z-(bi)modules.
instance ( AbelianGroup op a
         , add ~ Add
         , mul ~ Mul
         , op' ~ op
         ) =>
         LeftModule (Scalar op') add mul Integer op a

leftAnnihilates
  :: ( Eq a
     , Monoid Add a
     , LeftModule' (Scalar Add) r a
  )
  => r -> a -> Bool
leftAnnihilates r a = r <# a == zero

(<#)
  :: LeftActs (Scalar Add) r v
  => r -> v -> v
r <# v = leftAct' (undefined :: Proxy (Scalar Add)) r v

(<!)
  :: LeftActs (Self Mul) r r
  => r -> r -> r
r <! v = leftAct' (undefined :: Proxy (Self Mul)) r v

-- instance ( AbelianGroup op a
--          , Semigroup op Integer
--          , add ~ Add
--          , mul ~ Mul
--          , tag ~ Scalar op
--          ) =>
--          RightModule tag add mul Integer op a

-- instance (CommutativeRing add mul r, LeftModule (Scalar op) add mul r op a) =>
--          RightModule (Scalar op) add mul r op a

-- instance ( AbelianGroup op a
--          , rtag ~ Scalar op
--          , stag ~ Scalar op
--          , radd ~ Add
--          , rmul ~ Mul
--          , sadd ~ Add
--          , smul ~ Mul
--          ) =>
--          Bimodule rtag radd rmul Integer stag sadd smul Integer op a

-- type Bimodule_ rtag r stag s add mul v = Bimodule rtag add mul r stag add mul s Add v

-- instance (Ring add mul r, r ~ s, s ~ t, r ~ t, ltag ~ Self mul, rtag ~ Self mul) =>
--          Bimodule_ ltag r rtag s add mul t

-- a_
--   :: Bimodule_ (Self Mul) r (Self Mul) s Add Mul v
--   => r -> s -> v -> v
-- a_ r s v = leftAct p r $ rightAct p v s
--   where
--     p = (undefined :: Proxy (Self Mul))

-- val :: Double
-- val = 2 <! 3

-- -- :t f
-- -- f :: (Commutative 'Mul a, Commutative 'Add a,
-- --       DistributesOver 'Add 'Mul a, Neutral 'Mul a, Neutral 'Add a,
-- --       Semigroup 'Mul a, Semigroup 'Add a) =>
-- --      a -> a -> a

-- f :: CommutativeSemiring Add Mul a => a -> a -> a
-- f a b = a `plus` (b `plus` zero)

-- instance

-- type Bimodule' r s = Bimodule Add Mul r Add Mul s

-- info :: Q Info
-- d :: Type
-- d = lookupTypeName "Double"

-- instance Semiring Add Mul Double
-- instance ( Neutral add a
--          , Magma add a
--          , Neutral mul a
--          , Magma mul a
--          ) =>
--          Semiring add mul a



-- class Ring a where

-- class Ring a => Module a b | b -> a where

-- instance Ring a => Module a (Ring a) where

-- instance Module a b => Ring b where

-- a :: Double
-- a = zero `plus` zero

-- ooh
-- zero = b `plus` b
--   where b = neutral (undefined :: Proxy Add)

-- instance Ring Add Mul r => LeftModule (Self Mul) Add Mul r Mul r where

-- instance (Ring add mul r, AbelianGroup vadd v, LeftActs r ltag v) =>
--          LeftModule ltag add mul r vadd v


-- a :: Double
-- a =
--   zero `plus` (one `plus` one `plus` reciprocal one) `times`
--   (negate (one `plus` one `plus` one))

-- instance (AbelianGroup op a) =>
--          RightActs (Scalar op) Integer a where
--   rightAct = flip . leftAct

-- class (Semigroup op a) => LeftActs op a tag b where
--   leftAct :: Proxy tag -> Proxy op -> a -> b -> b

-- class (Semigroup op a) => RightActs op a tag b where
--   rightAct :: Proxy tag -> Proxy op -> b -> a -> b

-- -- instance (CommutativeSemigroup op a, RightActs tag a b, tag ~ Scalar op) =>
-- --          LeftActs tag a b where
-- --   leftAct = flip . rightAct

-- instance {-# OVERLAPPING #-}
--   (CommutativeSemigroup op a, LeftActs op a tag b, tag ~ Scalar op) =>
--          RightActs op a tag b where
--   rightAct p q = flip (leftAct p q)

-- -- | Abelian groups are Z-(bi)modules.
-- instance  (AbelianGroup op a) =>
--          LeftActs am Integer (Scalar op) a where
--   leftAct sproxy n' a =
--     let proxy = (undefined :: Proxy op)
--     in case n' of
--       0 -> neutral proxy
--       1 -> a
--       n -> {- FIXME slow! -} binaryOp proxy a (leftAct sproxy (n P.- 1) a)

-- -- instance (AbelianGroup op a) =>
-- --          RightActs (Scalar op) Integer a where
-- --   rightAct = flip . leftAct

-- -- | FIXME: tons of distributivity implied hereafter in the actions

-- class (LeftActs lop a ltag b, RightActs rop a rtag b) =>
--       Acts lop rop ltag rtag a b

-- instance (CommutativeSemigroup op a, RightActs tag a b, tag ~ Scalar op) =>
--          LeftActs tag a b where
--   leftAct = flip . rightAct

-- instance ( Commutative mul r
--          , LeftModule ltag add mul r vadd v
--          , RightActs ltag r v
--          ) =>
--          RightModule ltag add mul r vadd v
