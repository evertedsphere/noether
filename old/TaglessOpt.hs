{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Tagless where

import           Control.Arrow
import           Control.Category

import           Control.Monad.Reader
import           Control.Monad.State

import           Debug.Trace
import           Prelude              hiding (id, (.))

class NumP r where
  lit :: Int -> r
  add :: r -> r -> r
  mul :: r -> r -> r
  neg :: r -> r

instance NumP Int where
  lit = id
  add = (+)
  mul = (*)
  neg = negate

instance NumP String where
  lit = show
  add a b = "(" ++ a ++ " + " ++ b ++ ")"
  mul a b = "(" ++ a ++ " * " ++ b ++ ")"
  neg s = "(-" ++ s ++ ")"

term =
  add
    (lit 3)
    (add
       (neg (lit 4))
       (neg
          (add
             (lit 5)
             (mul
                (lit 5)
                (mul
                   (lit 5)
                   (neg
                      (neg
                         (add
                            (mul (neg (lit 1)) (neg (lit 2)))
                            (add (lit 0) (mul (lit 0) (lit 5)))))))))))

eval :: Int -> Int
eval = id

view :: String -> String
view = id

-- | "Negation pushdown" transformation

data ONegCtx = Pos | Neg

negCtx Pos = Neg
negCtx Neg = Pos

pushLit n Pos = lit n
pushLit n Neg = neg (lit n)

instance NumP a => NumP (ONegCtx -> a) where
  lit = pushLit
  neg e = e . negCtx
  mul a b ctx = mul (a ctx) (b ctx)
  add a b ctx = add (a ctx) (b ctx)

instance NumP a => NumP (Reader ONegCtx a) where
  lit = asks . pushLit
  neg e = asks (runReader e . negCtx)
  add a b = asks $ \ctx -> add (runReader a ctx) (runReader b ctx)
  mul a b = asks $ \ctx -> mul (runReader a ctx) (runReader b Pos)

simplifyNegs :: Reader ONegCtx t -> t
simplifyNegs e = runReader e Pos

-- Optimizations using properties of 0

data OZeroCtx = Zero | NotZero
  deriving (Show, Eq)

instance (NumP a) =>
         NumP (a, OZeroCtx) where
  lit n =
    ( lit n
    , if n == 0
        then Zero
        else NotZero)

  neg (_, Zero)    = (lit 0, Zero)
  neg (n, NotZero) = (neg n, NotZero)

  add (_, Zero) b               = b
  add a (_, Zero)               = a
  -- FIXME: what about a + (-a)?
  add (a, NotZero) (b, NotZero) = (add a b, NotZero)

  mul a@(_, Zero) _             = a
  mul _ b@(_, Zero)             = b
  mul (a, NotZero) (b, NotZero) = (mul a b, NotZero)

simplifyZeros :: (a, OZeroCtx) -> a
simplifyZeros = fst

loopy = add (lit 1) (mul (lit 1) (add (mul (lit 0) loopy) (lit 5)))

simplified = simplifyZeros . simplifyNegs
