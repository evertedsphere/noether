# Noether

Working towards a number theory / abstract algebra playground in Haskell. 

The part I'm working on at present develops a highly polymorphic numeric hierarchy, where typeclasses representing algebraic structures are "tagged" with the operations that the base type supports, with delightful consequences like

```haskell
instance {-# INCOHERENT #-}
         ( DotProductSpace' k v
         , DotProductSpace' k w
         , p ~ Add
         , m ~ Mul
         ) => InnerProductSpace DotProduct p m k Add (v, w) where
```

Obviously, I'm still exploring the design space here to try and find a good balance between avoiding arbitrary choices (e.g. no privileged `Monoid` instances for `Double` and the like) and a useful level of type inference. In large part, this means that I'm trying not to run up against trouble with instance resolution and failing hard (see above), or discovering that associated types are sometimes less permissive than one would like.

The numeric hierarchy, at present, extends to functions like this:

```haskell
(%<) :: LeftModule' r v => r -> v -> v
r %< v = leftAct AddP AddP MulP r v

-- | Linear interpolation.
-- lerp λ v w = λv + (1 - λ)w
lerp
  :: VectorSpace' r v
  => r -> v -> v -> v
lerp lambda v w = lambda %< v + w >% (one - lambda)

lol :: (Complex Double, Complex Double)
lol =
  (1, 3) * lerp lambda (3, 3) (4, 5) + (1, 0) >% lambda + v + lambda %< w +
  (lambda, -lambda)

  where
    lambda :: Complex Double
    lambda = 0.3 :+ 1

    v = (3, 3)
    w = (2, 7)
```

A preliminary implementation of linear maps between (what should be) free modules is being developed after the design in Conal Elliott's "Reimagining matrices". Some sample function signatures:

```haskell

-- | Converts a linear map into a function.
apply :: k \> a ~> b -> a -> b

compose
  :: k \> a ~> b
  -> k \>      b ~> c
  -> k \> a ~>      c
```

Usage looks like this for now:

```
> apply (rotate (pi / 4 :: Double)) (1,1)
(1.1102230246251565e-16,1.414213562373095)
```

The original core of the project is a short implementation of elliptic curve addition over Q works, which I've put on hold temporarily as I try to work out the issues outlined above first.

I'm using a Protolude "fork" called Lemmata that I expect will evolve over time. *subhask* is another inspiration for this project.
