# Noether

FULLPOLYMORPHIC™ number theory / abstract algebra in Haskell.

The part I'm working on at present develops a highly polymorphic numeric hierarchy. Unlike almost every other project (including the great `subhask`, which is by far the biggest inspiration for this project), all typeclasses representing algebraic structures are "tagged" with the operations that the base type supports. The intention is to have, without newtyping, things like automatically specified *L*-vector space instances for any *K*-vector space with *K / L* a (nice) field extension.

<!-- While it may not be inevitable, my inexperienced preliminary encoding of these ideas has delightful consequences like -->

<!-- ```haskell -->
<!-- instance {-# INCOHERENT #-} -->
<!--          ( DotProductSpace' k v -->
<!--          , DotProductSpace' k w -->
<!--          , p ~ Add -->
<!--          , m ~ Mul -->
<!--          ) => InnerProductSpace DotProduct p m k Add (v, w) where -->
<!-- ``` -->

<!-- that I don't know how to "kill with fire". -->

<!-- Obviously, I'm still exploring the design space to try and find a good balance between avoiding arbitrary choices (e.g. no privileged `Monoid` instances for `Double` and the like) and a useful level of type inference. In large part, this means that I'm trying not to run up against trouble with instance resolution and failing hard (see above), or discovering that associated types are sometimes less permissive than one would like. -->

<!-- The numeric hierarchy, at present, extends to functions like this: -->

<!-- ```haskell -->
<!-- (%<) :: LeftModule' r v => r -> v -> v -->
<!-- r %< v = leftAct AddP AddP MulP r v -->

<!-- -- | Linear interpolation. -->
<!-- -- lerp λ v w = λv + (1 - λ)w -->
<!-- lerp -->
<!--   :: VectorSpace' r v -->
<!--   => r -> v -> v -> v -->
<!-- lerp lambda v w = lambda %< v + w >% (one - lambda) -->

<!-- lol :: (Complex Double, Complex Double) -->
<!-- lol = -->
<!--   (1, 3) * lerp lambda (3, 3) (4, 5) + (1, 0) >% lambda + v + lambda %< w + -->
<!--   (lambda, -lambda) -->

<!--   where -->
<!--     lambda :: Complex Double -->
<!--     lambda = 0.3 :+ 1 -->

<!--     v = (3, 3) -->
<!--     w = (2, 7) -->
<!-- ``` -->

<!-- A preliminary implementation of linear maps between (what should be) free modules is being developed after the design in Conal Elliott's "Reimagining matrices". The added polymorphism and lack of fixed `Scalar a`-esque base fields is an interesting challenge, and Conal's basic GADT decomposition of linear maps changes in my case to -->

<!-- ```haskell -->
<!-- data (\>) :: (* -> * -> * -> *) where -->
<!-- ``` -->

<!-- where the first "slot" is for the base field. With a nice `~>` type operator (which is basically `$`), a linear map between two *k*-vector space types `a` and `b` has the type  -->

<!-- ```haskell -->
<!-- func :: k \> a ~> b -->
<!-- ``` -->

<!-- paving the way for the representation of the category _k_-**Vect** as `(\>) k :: (* -> * -> *)`. -->

<!-- Some sample function signatures: -->

<!-- ```haskell -->

<!-- -- | Converts a linear map into a function. -->
<!-- apply :: k \> a ~> b -> a -> b -->

<!-- compose -->
<!--   :: k \> a ~> b -->
<!--   -> k \>      b ~> c -->
<!--   -> k \> a ~>      c -->
<!-- ``` -->

<!-- Usage looks like this for now: -->

<!-- ``` -->
<!-- > apply (rotate (pi / 4 :: Double)) (1,1) -->
<!-- (1.1102230246251565e-16,1.414213562373095) -->
<!-- ``` -->

# Other stuff

Some other stuff I'm thinking about includes polymorphic numeric literals, possibly along the lines of this:

```haskell
type family NumericLit (n :: Nat) = (c :: * -> Constraint) where
  NumericLit 0 = Neutral Add
  NumericLit 1 = Neutral Mul
  -- NumericLit 2 = Field Add Mul
  -- NumericLit n = NumericLit (n - 1)
  NumericLit n = Ring Add Mul

fromIntegerP :: forall n a. (KnownNat n, NumericLit n a) => Proxy n -> a
fromIntegerP p =
  case sameNat p (Proxy :: Proxy 0) of
    Just prf -> gcastWith prf zero'
    Nothing -> case sameNat p (Proxy :: Proxy 1) of
      Just prf -> gcastWith prf one'
      Nothing -> undefined -- unsafeCoerce (val (Proxy :: Proxy a))
        -- where
        --   val :: (Field Add Mul b) => Proxy b -> b
        --   val _ = one + undefined -- fromIntegerP (Proxy :: Proxy (n - 1))
```

The original core of the project is a short implementation of elliptic curve addition over Q, which I've put on hold temporarily as I try to work out the issues outlined above first. This part uses a Protolude "fork" called Lemmata that I expect will evolve over time.
