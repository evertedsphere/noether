# Noether

Working towards a number theory playground in Haskell. Rudimentary elliptic curve addition over Q works for now.

I'm using a Protolude "fork" called Lemmata that I expect will evolve over time. *subhask* is another inspiration for this project.

A separate set of files develops a numeric hierarchy, which at present extends to this:

```haskell
-- | Linear interpolation.
lerp
  :: VectorSpace' r v
  => r -> v -> v -> v
lerp lambda v w = lambda >% v + w %< (one - lambda)

lol :: (Double, Double)
lol = lerp lambda (3, 3) (4, 5)
  where
    lambda :: Double
    lambda = 0.3
```
