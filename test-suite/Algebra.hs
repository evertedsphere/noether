{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import           Hedgehog.Internal.Property
import           Hedgehog.Internal.Show
import           Hedgehog.Internal.Source
import qualified Hedgehog.Range             as Range

import           Algebra.Modules
import           GHC.Stack
import           Prelude

genDouble :: Monad m => Test m Double
genDouble = forAll $ Gen.realFloat $ Range.linearFrac (-100) 100

almostEqual :: Double -> Double -> Bool
almostEqual a b = abs (a - b) < 0.005

(=~=) :: (Monad m, HasCallStack) => (Double,Double) -> (Double,Double) -> Test m ()
(=~=) x y =
  if (fst x `almostEqual` fst y) && (snd x `almostEqual` snd y)
    then success
    else case valueDiff <$> mkValue x <*> mkValue y of
           Nothing ->
             withFrozenCallStack $
             failWith Nothing $
             unlines ["━━━ Not Equal ━━━", showPretty x, showPretty y]
           Just diff ->
             withFrozenCallStack $
             failWith (Just $ Diff "Failed (" "- lhs" "=/=" "+ rhs" ")" diff) ""


prop_lerp_0 :: Property
prop_lerp_0 =
  property $ do
    v <- (,) <$> genDouble <*> genDouble
    w <- (,) <$> genDouble <*> genDouble
    lerp @Double 0 v w =~= w

prop_lerp_1 :: Property
prop_lerp_1 =
  property $ do
    v <- (,) <$> genDouble <*> genDouble
    w <- (,) <$> genDouble <*> genDouble
    lerp @Double 1 v w =~= v

tests :: IO ()
tests = do
  checkParallel $$discover
  pure ()
