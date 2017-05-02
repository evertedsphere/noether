import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import           Algebra
import           EllipticCurve
import           Lemmata        hiding (one, zero, (*), (+), (/))

getRational :: (Monad m) => Test m Rational
getRational = forAll $ Gen.realFrac_ $ Range.linearFrac 1.0 100.0

prop_rp2_eq_refl :: Property
prop_rp2_eq_refl =
  property $ do
    a <- getRational
    b <- getRational
    P2 a b === P2 a b

prop_rp2_eq_1 :: Property
prop_rp2_eq_1 =
  property $ do
    a <- getRational
    b <- getRational
    lambda <- getRational
    P2 a b === P2 (lambda * a) (lambda * b)

prop_rp2_eq_2 :: Property
prop_rp2_eq_2 =
  property $ do
    a <- getRational
    b <- getRational
    P2 a 0 === P2 b 0

prop_rp2_eq_3 :: Property
prop_rp2_eq_3 =
  property $ do
    a <- getRational
    b <- getRational
    P2 0 a === P2 0 b

prop_rp2_eq_4 :: Property
prop_rp2_eq_4 =
  property $ do
    a <- getRational
    b <- getRational
    c <- getRational
    assert $ P2 a c /= P2 b 0

prop_rp3_eq_1 :: Property
prop_rp3_eq_1 =
  property $ do
    a <- getRational
    b <- getRational
    lambda <- getRational
    P3 a b 0 === P3 (lambda * a) (lambda * b) 0

prop_rp3_eq_2 :: Property
prop_rp3_eq_2 =
  property $ do
    a <- getRational
    b <- getRational
    lambda <- getRational
    P3 0 a b === P3 0 (lambda * a) (lambda * b)

prop_rp3_eq_3 :: Property
prop_rp3_eq_3 =
  property $ do
    a <- getRational
    b <- getRational
    lambda <- getRational
    P3 a 0 b === P3 (lambda * a) 0 (lambda * b)

tests :: IO ()
tests = do
  checkParallel' $
    Group
      "Real projective space : order 2 : equality"
      [ ("[x : y] == [ x :  y]", prop_rp2_eq_refl)
      , ("[x : y] == [ax : ay]", prop_rp2_eq_1)
      , ("[a : 0] == [ b :  0]", prop_rp2_eq_2)
      , ("[0 : a] == [ 0 :  b]", prop_rp2_eq_3)
      , ("[a : b] /= [ c :  0]", prop_rp2_eq_4)
      ]
  putText ""
  checkParallel' $
    Group
      "Real projective space : order 3 : equality"
      [ ("[x : y : 0] == [ax : ay :  0]", prop_rp3_eq_1)
      , ("[0 : y : z] == [ 0 : ay : az]", prop_rp3_eq_2)
      , ("[x : 0 : z] == [ax :  0 : az]", prop_rp3_eq_3)
      ]
  where
    checkParallel' = void . checkParallel

main :: IO ()
main = tests

-- -- Tasty makes it easy to test your code. It is a test framework that can
-- -- combine many different types of tests into one suite. See its website for
-- -- help: <http://documentup.com/feuerbach/tasty>.
-- import qualified Test.Tasty
-- -- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- -- writing tests. Its website has more info: <https://hspec.github.io>.
-- import Test.Tasty.Hspec

-- main :: IO ()
-- main = do
--     test <- testSpec "noether" spec
--     Test.Tasty.defaultMain test

-- spec :: Spec
-- spec = parallel $ do
--     it "is trivially true" $ do
--         True `shouldBe` True
