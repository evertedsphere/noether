{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
module Noether.Test.Algebra where

import           Hedgehog                   hiding (Group)
import qualified Hedgehog.Gen               as Gen
import           Hedgehog.Internal.Property hiding (Group)
import qualified Hedgehog.Internal.Property as Prop
import           Hedgehog.Internal.Show
import           Hedgehog.Internal.Source
import qualified Hedgehog.Range             as Range

import           GHC.Stack
import qualified Prelude                    as P

import           Noether.Algebra.Single
import           Noether.Algebra.Tags
import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

genDouble
  :: Monad m
  => Test m Double
genDouble = forAll $ Gen.realFloat $ Range.linearFrac (-100) 100

almostEqual
  :: (Ord a, Fractional a)
  => a -> a -> Bool
almostEqual a b = abs (a P.- b) < 0.005

(=~=)
  :: (Monad m, HasCallStack, Ord a, Fractional a, Show a)
  => a -> a -> Test m ()
(=~=) x y =
  if (x `almostEqual` y)
    then success
    else withFrozenCallStack <$>
         maybe
           (failWith
              Nothing
              (unlines ["━━━ Not Equal ━━━", showPretty x, showPretty y]))
           (\diff ->
              failWith
                (Just (Diff "Failed (" "- lhs" "=/=" "+ rhs" ")" diff))
                "")
           (valueDiff <$> mkValue x <*> mkValue y)

-- | Create a property for an 'Integral' type.
mkProp_integral
  :: forall a t.
     (Integral a, Show a)
  => t -> (a -> a -> a) -> (a -> a -> a) -> (t, Property)
mkProp_integral name preludeOp noetherOp =
  namedProperty
    name
    (do let r = forAll (Gen.integral (Range.linear (-100) 100))
        a <- r
        b <- r
        (a `preludeOp` b) === (a `noetherOp` b))

prop_prelude_add_int :: (PropertyName, Property)
prop_prelude_add_int = mkProp_integral @Int "(+) : Int" (P.+) (+)

prop_prelude_mul_int :: (PropertyName, Property)
prop_prelude_mul_int = mkProp_integral @Int "(*) : Int" (P.*) (*)

prop_prelude_sub_int :: (PropertyName, Property)
prop_prelude_sub_int = mkProp_integral @Int "(-) : Int" (P.-) (-)

prop_prelude_add_integer :: (PropertyName, Property)
prop_prelude_add_integer = mkProp_integral @Integer "(+) : Integer" (P.+) (+)

prop_prelude_mul_integer :: (PropertyName, Property)
prop_prelude_mul_integer = mkProp_integral @Integer "(*) : Integer" (P.*) (*)

prop_prelude_sub_integer :: (PropertyName, Property)
prop_prelude_sub_integer = mkProp_integral @Integer "(-) : Integer" (P.-) (-)

-- | Create a property for a 'RealFloat' type.
mkProp_realFloat
  :: (RealFloat a, Ord a, Show a)
  => t
  -> (a -> a -> a)
  -> (a -> a -> a)
  -> (t, Property)
mkProp_realFloat name preludeOp noetherOp =
  namedProperty
    name
    (do let r = forAll (Gen.realFloat (Range.linearFrac (-100) 100))
        a <- r
        b <- r
        (a `preludeOp` b) =~= (a `noetherOp` b))

prop_prelude_add_float :: (PropertyName, Property)
prop_prelude_add_float = mkProp_realFloat @Float "(+) : Float" (P.+) (+)

prop_prelude_mul_float :: (PropertyName, Property)
prop_prelude_mul_float = mkProp_realFloat @Float "(*) : Float" (P.*) (*)

prop_prelude_sub_float :: (PropertyName, Property)
prop_prelude_sub_float = mkProp_realFloat @Float "(-) : Float" (P.-) (-)

prop_prelude_div_float :: (PropertyName, Property)
prop_prelude_div_float = mkProp_realFloat @Float "(/) : Float" (P./) (/)

prop_prelude_add_double :: (PropertyName, Property)
prop_prelude_add_double = mkProp_realFloat @Double "(+) : Double" (P.+) (+)

prop_prelude_mul_double :: (PropertyName, Property)
prop_prelude_mul_double = mkProp_realFloat @Double "(*) : Double" (P.*) (*)

prop_prelude_sub_double :: (PropertyName, Property)
prop_prelude_sub_double = mkProp_realFloat @Double "(-) : Double" (P.-) (-)

prop_prelude_div_double :: (PropertyName, Property)
prop_prelude_div_double = mkProp_realFloat @Double "(/) : Double" (P./) (/)

namedProperty name prop = (name, property prop)

tests :: IO ()
tests = do
  checkParallel'
    "Noether numerics agree with Prelude ops"
    [ prop_prelude_add_int
    , prop_prelude_sub_int
    , prop_prelude_mul_int
    , prop_prelude_add_integer
    , prop_prelude_sub_integer
    , prop_prelude_mul_integer
    , prop_prelude_add_float
    , prop_prelude_sub_float
    , prop_prelude_mul_float
    , prop_prelude_div_float
    , prop_prelude_add_double
    , prop_prelude_sub_double
    , prop_prelude_mul_double
    , prop_prelude_div_double
    ]
  putStrLn "asdf"
  where
    checkParallel' name tests = checkParallel (Prop.Group name tests)
