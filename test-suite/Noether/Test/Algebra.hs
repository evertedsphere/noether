{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Noether.Test.Algebra where

import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import           Hedgehog.Internal.Property as Prop
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

almostEqual :: Double -> Double -> Bool
almostEqual a b = abs (a - b) < 0.005

(=~=)
  :: (Monad m, HasCallStack)
  => Double -> Double -> Test m ()
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

prop_prelude_add_int :: (PropertyName, Property)
prop_prelude_add_int = mkProp_int @Int "(+) : Int" (P.+) (+)

prop_prelude_mul_int :: (PropertyName, Property)
prop_prelude_mul_int = mkProp_int @Int "(*) : Int" (P.*) (*)

prop_prelude_sub_int :: (PropertyName, Property)
prop_prelude_sub_int = mkProp_int @Int "(-) : Int" (P.-) (-)

mkProp_int
  :: forall a t.
     (Integral a, Show a)
  => t -> (a -> a -> a) -> (a -> a -> a) -> (t, Property)
mkProp_int name preludeOp noetherOp =
  namedProperty
    name
    (do let r = forAll (Gen.integral (Range.linear (-100) 100))
        a <- r
        b <- r
        (a `preludeOp` b) === (a `noetherOp` b))

mkProp_double
  :: t
  -> (Double -> Double -> Double)
  -> (Double -> Double -> Double)
  -> (t, Property)
mkProp_double name preludeOp noetherOp =
  namedProperty
    name
    (do let r = forAll (Gen.double (Range.linearFrac (-100) 100))
        a <- r
        b <- r
        (a `preludeOp` b) =~= (a `noetherOp` b))

prop_prelude_add_double :: (PropertyName, Property)
prop_prelude_add_double = mkProp_double "(+) : Double" (P.+) (+)

prop_prelude_mul_double :: (PropertyName, Property)
prop_prelude_mul_double = mkProp_double "(*) : Double" (P.*) (*)

prop_prelude_sub_double :: (PropertyName, Property)
prop_prelude_sub_double = mkProp_double "(-) : Double" (P.-) (-)

prop_prelude_div_double :: (PropertyName, Property)
prop_prelude_div_double = mkProp_double "(/) : Double" (P./) (/)

namedProperty name prop = (name, property prop)

-- prop_lerp_0 :: Property
-- prop_lerp_0 =
--   property $ do
--     v <- (,) <$> genDouble <*> genDouble
--     w <- (,) <$> genDouble <*> genDouble
--     lerp @Double 0 v w =~= w

-- prop_lerp_1 :: Property
-- prop_lerp_1 =
--   property $ do
--     v <- (,) <$> genDouble <*> genDouble
--     w <- (,) <$> genDouble <*> genDouble
--     lerp @Double 1 v w =~= v

tests :: IO ()
tests = do
  checkParallel'
    "Noether numerics agree with Prelude ops"
    [ prop_prelude_add_int
    , prop_prelude_sub_int
    , prop_prelude_mul_int
    , prop_prelude_add_double
    , prop_prelude_sub_double
    , prop_prelude_mul_double
    , prop_prelude_div_double
    ]
  putStrLn "asdf"
  where
    checkParallel' name tests = checkParallel (Prop.Group name tests)
