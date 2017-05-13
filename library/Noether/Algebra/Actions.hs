{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskellQuotes  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Algebra.Actions where

import           Algebra.Basics
import           Data.Kind (type (*))
import           Data.Proxy
import           Prelude        hiding (Monoid, negate, recip, (*), (+), (-),
                                 (/), fromInteger)
import qualified Prelude        as P
import Data.Complex

-- I could see this turning into a (type|data) family
data IsLinear     = YesLinear     | NotLinear
data IsCompatible = YesCompatible | NotCompatible

-- | Associative/semigroup actions

class
  ( Semigroup ao a
  , Semigroup bo b
  ) => LeftActs
       (ao :: BinaryTag) (a :: *)
       (bo :: BinaryTag) (b :: *)
       (action :: BinaryTag)
       (linear :: IsLinear)
       (compatible :: IsCompatible)
  | ao a bo b action -> linear compatible
  where

  leftAct :: Proxy ao -> Proxy bo -> Proxy action -> a -> b -> b

  default leftAct
    :: (a ~ b, Semigroup action a)
    => Proxy ao -> Proxy bo -> Proxy action -> a -> b -> b
  leftAct _ _ = binaryOp

class
  ( Semigroup ao a
  , Semigroup bo b
  ) => RightActs
       (ao :: BinaryTag) (a :: *)
       (bo :: BinaryTag) (b :: *)
       (action :: BinaryTag)
       (linear :: IsLinear)
       (compatible :: IsCompatible)
  | ao a bo b action -> linear compatible
  where

  rightAct :: Proxy ao -> Proxy bo -> Proxy action -> a -> b -> b

  default rightAct
    :: (a ~ b, Semigroup action a)
    => Proxy ao -> Proxy bo -> Proxy action -> a -> b -> b
  rightAct _ _ = binaryOp

type LeftLinear       ao a bo b t = LeftActs  ao a bo b t YesLinear NotCompatible
type RightLinear      ao a bo b t = RightActs ao a bo b t YesLinear NotCompatible

type LeftCompatible   ao a bo b t = LeftActs  ao a bo b t NotLinear YesCompatible
type RightCompatible  ao a bo b t = RightActs ao a bo b t NotLinear YesCompatible

instance (Ring Add m a) => LeftActs  Add a Add a m   YesLinear NotCompatible
instance (Ring Add m a) => RightActs Add a Add a m   YesLinear NotCompatible

instance (Ring p Mul a) => LeftActs  Mul a p   a Mul NotLinear YesCompatible
instance (Ring p Mul a) => RightActs Mul a p   a Mul NotLinear YesCompatible

scale :: Double -> Complex Double -> Complex Double
scale d (a :+ b) = (d * a) :+ (d * b)

instance LeftActs  Add Double Add (Complex Double) Mul YesLinear NotCompatible where
  leftAct _ _ _ = scale
instance RightActs Add Double Add (Complex Double) Mul YesLinear NotCompatible where
  rightAct _ _ _ = scale

instance LeftActs  Mul Double Add (Complex Double) Mul NotLinear YesCompatible where
  leftAct _ _ _ = scale
instance RightActs Mul Double Add (Complex Double) Mul NotLinear YesCompatible where
  rightAct _ _ _ = scale

-- instance ( LeftActs Add k Add a m YesLinear NotCompatible
--          , LeftActs Add k Add b m YesLinear NotCompatible) =>
--          LeftActs Add k Add (a, b) m YesLinear NotCompatible where
--   leftAct p1 p2 p3 s (t, t') = (leftAct p1 p2 p3 s t, leftAct p1 p2 p3 s t')

instance ( LeftActs Add a Add b m YesLinear NotCompatible
         , LeftActs Add a Add c m YesLinear NotCompatible
         ) =>
         LeftActs Add a Add (b, c) m YesLinear NotCompatible where
  leftAct p1 p2 p3 s (t, t') = (leftAct p1 p2 p3 s t, leftAct p1 p2 p3 s t')

instance ( RightActs Add a Add b m YesLinear NotCompatible
         , RightActs Add a Add c m YesLinear NotCompatible
         ) =>
         RightActs Add a Add (b, c) m YesLinear NotCompatible where
  rightAct p1 p2 p3 s (t, t') = (rightAct p1 p2 p3 s t, rightAct p1 p2 p3 s t')

instance ( LeftActs Mul a p b Mul NotLinear YesCompatible
         , LeftActs Mul a p c Mul NotLinear YesCompatible
         ) =>
         LeftActs Mul a p (b, c) Mul NotLinear YesCompatible where
  leftAct p1 p2 p3 s (t, t') = (leftAct p1 p2 p3 s t, leftAct p1 p2 p3 s t')

instance ( RightActs Mul a p b Mul NotLinear YesCompatible
         , RightActs Mul a p c Mul NotLinear YesCompatible
         ) =>
         RightActs Mul a p (b, c) Mul NotLinear YesCompatible where
  rightAct p1 p2 p3 s (t, t') = (rightAct p1 p2 p3 s t, rightAct p1 p2 p3 s t')

abelianIntegerAction
  :: (AbelianGroup op a)
  => Proxy add -> Proxy op -> Proxy mul -> Integer -> a -> a
abelianIntegerAction _ opProxy _ 0 _ = neutral opProxy
abelianIntegerAction _ _ _ 1 a = a
abelianIntegerAction addProxy opProxy mulProxy n a =
  binaryOp opProxy a (abelianIntegerAction addProxy opProxy mulProxy (n P.- 1) a)

instance (AbelianGroup op a) => LeftActs  Add Integer op a Mul YesLinear NotCompatible where
  leftAct  = abelianIntegerAction
instance (AbelianGroup op a) => RightActs Add Integer op a Mul YesLinear NotCompatible where
  rightAct = abelianIntegerAction

instance (AbelianGroup op a) => LeftActs  Mul Integer op a Mul NotLinear YesCompatible where
  leftAct  = abelianIntegerAction
instance (AbelianGroup op a) => RightActs Mul Integer op a Mul NotLinear YesCompatible where
  rightAct = abelianIntegerAction
