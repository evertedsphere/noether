{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}

module EllipticCurve where


import           Lemmata                  hiding (Monoid, Semigroup, Semiring,
                                           fromInteger, negate, one, show, zero,
                                           (*), (+), (-), (/))

import qualified Lemmata                  as L

import           Data.Kind
import           Data.Proxy
import           Data.Reflection
import           GHC.Show
import           Noether.Algebra.Multiple
import           Noether.Algebra.Single

data P1 a = P1 a a
  deriving Show

data P2 a = P2 a a a
  deriving Show

-- instance Show (P2 Rational) where
--   show (P2 x y z) =
--     "[" <> show' x <> " : " <> show' y <> " : " <> show' z <> "]"
--     where
--       show' (0 :% _) = "0"
--       show' (n :% 1) = show n
--       show' (a :% b) = show a <> "/" <> show b

-- -- deriving instance Show a => Show (P2 a)

data A2 a = A2 a a
  deriving Show

data A3 a = A3 a a a
  deriving Show

zero' :: Field a => P2 a
zero' = P2 zero one zero

instance (Field a, Eq a) =>
         Eq (P1 a) where
  (P1 a b) == (P1 d e)
    | a == zero && b /= zero = (d == zero) && (e /= zero)
    | a /= zero && b == zero = (d /= zero) && (e == zero)
    | otherwise = (d /= zero) && (e /= zero) && (a / d == b / e)

instance (Field a, Eq a) =>
         Eq (P2 a) where
  (P2 a b c) == (P2 d e f)
    | a == zero && b /= zero =
      (d == zero) && (e /= zero) && P2 b a c == P2 e d f
    | a == zero && c /= zero =
      (d == zero) && (f /= zero) && P2 c b a == P2 f e d
    | b == zero && c /= zero =
      (e == zero) && (f /= zero) && P2 a c b == P2 d f e
    -- now we have a > b > c ito zeroness, a nonzero
    | c == zero = (f == zero) && (P1 a b == P1 d e)
    | otherwise =
      (d /= zero) &&
      (e /= zero) && (f /= zero) && a / d == b / e && b / e == c / f

dehomogenize :: Field a => P2 a -> A2 a
dehomogenize (P2 a b c) = A2 (a / c) (b / c)

-- | Weierstrass model over a field.
-- | This denotes an elliptic curve y^2 = x^3 + ax + b.
data WM a = WM a a
  deriving Show

inf :: Field k => P2 k
inf = P2 zero one zero

exampleCurve :: Num k => WM k
exampleCurve = WM 2 3

-- | char k /= 2 or 3
ellipticPlus
  :: (Field k, Eq k, Num k)
  => WM k -> P2 k -> P2 k -> P2 k
ellipticPlus (WM a _) p q
  | p == inf = q
  | q == inf = p
  | otherwise =
    let A2 xp yp = dehomogenize p
        A2 xq yq = dehomogenize q
    in if xp /= xq
         then let s = (yp - yq) / (xp - xq)
                  x = s * s - xp - xq
                  y = yp + s * (x - xp)
              in ecNegate (P2 x y one)
         else if yp + yq == zero
                then inf
                else let s = (3 * xp ^ 2 + a) / (2 * yp)
                         x = s * s - 2 * xp
                         y = yp + s * (x - xp)
                     in ecNegate (P2 x y one)

discriminant :: (Field k, Num k) => WM k -> k
discriminant (WM a b) = -16 * (4 * a ^ 3 + 27 * b ^ 2)

onCurve :: (Show k, Field k, Eq k, Num k) => WM k -> P2 k -> Bool
onCurve (WM a b) p
  | p == inf = True
  | otherwise = y ^ 2 == x ^ 3 + a * x + b
  where
    (A2 x y) = dehomogenize p

ecNegate :: (Field a, Eq a) => P2 a -> P2 a
ecNegate p@(P2 a b c)
  | p == inf = inf
  | otherwise = P2 a (negate b) c

-- Reflection trickery ahead.
-- You have been warned.
-- h/t https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection

newtype WM' k = WM' { unWM' :: WM k }

newtype CurvePt k s = CurvePt { unCurvePt :: P2 k }

type HasWM k s = Reifies s (WM' k)
type CurvePt' k = forall (s :: *). HasWM k s => CurvePt k s

instance (Field k, Eq k, Num k, HasWM k s) => Semigroup (CurvePt k s) where
  p + q = CurvePt $ ellipticPlus (unWM' $ reflect p) (unCurvePt p) (unCurvePt q)

instance (Field k, Eq k, Num k, HasWM k s) => Monoid (CurvePt k s) where
  zero = CurvePt inf

instance (Field k, Eq k, Num k, HasWM k s) => Cancellative (CurvePt k s) where
  p - q = p + negate q

instance (Field k, Eq k, Num k, HasWM k s) => Group (CurvePt k s) where
  negate = CurvePt . ecNegate . unCurvePt

-- instance (Field k, Eq k, Num k, HasWM k s) => AbelianGroup (CurvePt k s)

computeOver
  :: (Field k, Eq k, Num k)
  => WM k
  -> CurvePt' k
  -> P2 k
computeOver wm ec = reify (WM' wm) (unCurvePt . asProxyOf ec)
  where
    asProxyOf :: f s -> Proxy s -> f s
    asProxyOf e _ = e

pt :: a -> a -> a -> CurvePt a s
pt a b c = CurvePt (P2 a b c)

liftEC :: P2 a -> CurvePt a s
liftEC = CurvePt

-- sum' :: P2 QQ
-- sum' = computeOver curve $
--   let a = pt 2 3 5
--       b = pt 3 5 6
--       c = negate $ pt 3 5 6
--   in a + b + c

slowPow :: (Field k, Eq k, Num k) => CurvePt' k -> Int -> CurvePt' k
slowPow _ 0 = zero
slowPow p 1 = p
slowPow p 2 = p + p
slowPow p n
  | n > 2 =
    let (d, m) = n `divMod` 2
        next = slowPow (slowPow p d) 2
    in if m == 0
         then next
         else next + p
  | n < 0 = negate (slowPow p n)

slowPow' :: (Field k, Eq k, Num k) => WM k -> P2 k -> Int -> P2 k
slowPow' curve p n = computeOver curve (slowPow (liftEC p) n)

canonicalize (P2 (xn :% xd) (yn :% yd) (zn :% zd)) =
  canonicalize' (P2 (xn * yd * zd) (yn * xd * zd) (zn * xd * yd))

canonicalize' (P2 x y z) =
  let g = gcd x (gcd y z)
  in P2 (x `div` g) (y `div` g) (z `div` g)
