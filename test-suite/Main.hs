import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

-- import           Algebra
-- import           EllipticCurve
-- import           Lemmata        hiding (negate, one, zero, (*), (+), (-), (/))

import Noether.Test.Algebra

main :: IO ()
main = tests

-- genInt :: (Monad m, Num a) => Gen m a
-- genInt = map fromIntegral $ Gen.integral (Range.linearFrom 0 (-100) 100)

-- genIntNonzero :: (Monad m, Eq a, Num a) => Gen m a
-- genIntNonzero = Gen.filter (/=0) genInt

-- genRational :: (Monad m) => Gen m Rational
-- genRational = (/) <$> genInt <*> genIntNonzero

-- genNonzero :: (Monad m) => Gen m Rational
-- genNonzero = (/) <$> genIntNonzero <*> genIntNonzero

-- testNonzero :: (Monad m) => Test m Rational
-- testNonzero = forAll genNonzero

-- genP1 :: Monad m => Gen m (P1 Rational)
-- genP1 = Gen.filter nonsingular $ P1 <$> genRational <*> genRational
--   where
--     nonsingular (P1 a b) = a /= 0 && b /= 0

-- testP1 :: Monad m => Test m (P1 Rational)
-- testP1 = forAll genP1

-- scale :: Rg r => r -> P1 r -> P1 r
-- scale lambda (P1 a b) = P1 (lambda * a) (lambda * b)

-- genP2 :: Monad m => Gen m (P2 Rational)
-- genP2 = Gen.filter nonsingular $ P2 <$> genRational <*> genRational <*> genRational
--   where
--     nonsingular (P2 a b c) = a /= 0 || b /= 0 || c /= 0

-- testP2 :: Monad m => Test m (P2 Rational)
-- testP2 = forAll genP2

-- genEC :: Monad m => WM Rational -> Gen m (P2 Rational)
-- genEC wm =
--   Gen.filter (\p -> nonsingular p && onCurve wm p) $
--   P2 <$> genRational <*> genRational <*> genRational
--   where
--     nonsingular (P2 a b c) = (a /= 0 && c /= 0) || (a == 0 && c == 0 && b /= 0)

-- testEC :: Monad m => WM Rational -> Test m (P2 Rational)
-- testEC wm = forAll (genEC wm)

-- -- genCurvePt
-- --   :: (Typeable s, Monad m)
-- --   => Gen m (CurvePt Rational s)
-- -- genCurvePt = pt <$> genNonzero <*> genNonzero <*> genNonzero

-- -- testCurvePt
-- --   :: (Typeable s, Monad m)
-- --   => Test m (CurvePt Rational s)
-- -- testCurvePt = forAll genCurvePt

-- genCurve :: Monad m => Gen m (WM Rational)
-- genCurve = Gen.filter ((/= 0) . discriminant) (WM <$> genNonzero <*> genNonzero)

-- testCurve :: Monad m => Test m (WM Rational)
-- testCurve = forAll genCurve

-- prop_rp1_eq_refl :: Property
-- prop_rp1_eq_refl =
--   property $ do
--     a <- testP1
--     a === a

-- prop_rp1_eq_1 :: Property
-- prop_rp1_eq_1 =
--   property $ do
--     p <- testP1
--     lambda <- testNonzero
--     p === scale lambda p

-- prop_rp1_eq_2 :: Property
-- prop_rp1_eq_2 =
--   property $ do
--     a <- testNonzero
--     b <- testNonzero
--     P1 a 0 === P1 b 0

-- prop_rp1_eq_3 :: Property
-- prop_rp1_eq_3 =
--   property $ do
--     a <- testNonzero
--     b <- testNonzero
--     P1 0 a === P1 0 b

-- prop_rp1_eq_4 :: Property
-- prop_rp1_eq_4 =
--   property $ do
--     a <- testNonzero
--     b <- testNonzero
--     c <- testNonzero
--     assert $ P1 a c /= P1 b 0

-- prop_rp2_eq_1 :: Property
-- prop_rp2_eq_1 =
--   property $ do
--     a <- testNonzero
--     b <- testNonzero
--     lambda <- testNonzero
--     P2 a b 0 === P2 (lambda * a) (lambda * b) 0

-- prop_rp2_eq_2 :: Property
-- prop_rp2_eq_2 =
--   property $ do
--     a <- testNonzero
--     b <- testNonzero
--     lambda <- testNonzero
--     P2 0 a b === P2 0 (lambda * a) (lambda * b)

-- prop_rp2_eq_3 :: Property
-- prop_rp2_eq_3 =
--   property $ do
--     a <- testNonzero
--     b <- testNonzero
--     lambda <- testNonzero
--     P2 a 0 b === P2 (lambda * a) 0 (lambda * b)

-- ellipticCurveProperty :: Test IO () -> Property
-- ellipticCurveProperty = withTests 20 . withDiscards 10000 . property

-- prop_ec_plus_id_left :: Property
-- prop_ec_plus_id_left =
--   ellipticCurveProperty $ do
--     k <- testCurve
--     a' <- testEC k
--     let a, z :: CurvePt Rational s
--         a = liftEC a'
--         z = liftEC inf
--         lhs = computeOver k $ a + z
--         rhs = computeOver k a
--     lhs === rhs

-- prop_ec_plus_id_right :: Property
-- prop_ec_plus_id_right =
--   ellipticCurveProperty $ do
--     k <- testCurve
--     a' <- testEC k
--     let a, z :: CurvePt Rational s
--         a = liftEC a'
--         z = liftEC inf
--         lhs = computeOver k $ z + a
--         rhs = computeOver k a
--     lhs === rhs

-- prop_ec_plus_inverses :: Property
-- prop_ec_plus_inverses =
--   ellipticCurveProperty $ do
--     k <- testCurve
--     p <- testEC k
--     let a, z :: CurvePt Rational s
--         a = liftEC p
--         z = liftEC inf
--         lhs = computeOver k $ a + (negate a)
--         rhs = computeOver k z
--     lhs === rhs

-- prop_ec_plus_sym :: Property
-- prop_ec_plus_sym =
--   ellipticCurveProperty $ do
--     k <- testCurve
--     a' <- testEC k
--     b' <- testEC k
--     let a, b :: CurvePt Rational s
--         a = liftEC a'
--         b = liftEC b'
--         lhs = computeOver k $ a + b
--         rhs = computeOver k $ b + a
--     lhs === rhs

-- prop_ec_plus_regression_1 :: Property
-- prop_ec_plus_regression_1 =
--   ellipticCurveProperty $ do
--     k <- testCurve
--     a' <- testEC k
--     b' <- testEC k
--     let a, b :: CurvePt Rational s
--         a = liftEC a'
--         b = liftEC b'
--         lhs = computeOver k $ a + (b - a)
--         rhs = computeOver k $ (a + b) - a
--     lhs === rhs

-- prop_ec_plus_assoc :: Property
-- prop_ec_plus_assoc =
--   ellipticCurveProperty $ do
--     k <- testCurve
--     a' <- testEC k
--     b' <- testEC k
--     c' <- testEC k
--     let a, b, c :: CurvePt Rational s
--         a = liftEC a'
--         b = liftEC b'
--         c = liftEC c'
--         lhs = computeOver k $ a + (b + c)
--         rhs = computeOver k $ (a + b) + c
--     lhs === rhs

-- tests :: IO ()
-- tests = do
--   putText "\n -> Projective spaces\n"

--   checkParallel' $
--     Group
--       "Real projective space : order 2 : equality"
--       [ ("[x : y] == [ x :  y]", prop_rp1_eq_refl)
--       , ("[x : y] == [ax : ay]", prop_rp1_eq_1)
--       , ("[a : 0] == [ b :  0]", prop_rp1_eq_2)
--       , ("[0 : a] == [ 0 :  b]", prop_rp1_eq_3)
--       , ("[a : b] /= [ c :  0]", prop_rp1_eq_4)
--       ]
--   checkParallel' $
--     Group
--       "Real projective space : order 3 : equality"
--       [ ("[x : y : 0] == [ax : ay :  0]", prop_rp2_eq_1)
--       , ("[0 : y : z] == [ 0 : ay : az]", prop_rp2_eq_2)
--       , ("[x : 0 : z] == [ax :  0 : az]", prop_rp2_eq_3)
--       ]

--   putText "\n -> Elliptic curves\n"

--   checkParallel' $
--     Group
--       "Elliptic curves : group law : axioms"
--       [ ("P + 0 = P", prop_ec_plus_id_right)
--       , ("0 + P = P", prop_ec_plus_id_left)
--       , ("P + (-P)    =  0         ", prop_ec_plus_inverses)
--       , ("P +  Q      =  Q + P     ", prop_ec_plus_sym)
--       , ("P + (Q + R) = (P + Q) + R", prop_ec_plus_assoc)
--       ]
--   checkParallel' $
--     Group
--       "Elliptic curves : group law : regression tests"
--       [("(P + Q) - P = P + (Q - P)", prop_ec_plus_regression_1)]

--   where
--     checkParallel' = void . checkParallel

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
