{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module AdvancedOverlap where

import           GHC.Exts
import           Prelude  hiding (printN)

data EqNum -- Just two
data ApproximateEq
data NoShow -- distinct types
data Explicit
data Coerced a b

data Joke
data NoJoke

--- 1

type family Combine a b where
  Combine EqNum EqNum = EqNum
  Combine a b = a

class EqWith s a where
  useStrategy :: s -> a -> a -> Bool

instance (Eq a, Num a) => EqWith EqNum a where
  useStrategy _ = (==)

instance (EqWith s a, EqWith t b) =>
         EqWith (s, t) (a, b) where
  useStrategy _ (a, b) (a', b') =
    (useStrategy (undefined :: s) a a') && (useStrategy (undefined :: t) b b')

instance (Num a, Ord a, Floating a) => EqWith ApproximateEq a where
  useStrategy _ x y = abs (x - y) < 0.01

instance EqWith Explicit Int where
  useStrategy _ _ _ = False

instance EqWith Explicit Char where
  useStrategy _ _ _ = False

-- | The concrete equality strategy for a type.
type family EqStrategy a

type instance EqStrategy Double = ApproximateEq
type instance EqStrategy (Double, Double) = (ApproximateEq, EqNum)

class Eeekew a where
  (===) :: a -> a -> Bool

instance (EqStrategy a ~ s, EqWith s a) => Eeekew a where
  (===) = useStrategy (undefined :: s)
