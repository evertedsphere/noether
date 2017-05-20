module Noether.Algebra.Actions.Strategies where

import           Noether.Algebra.Single
import           Noether.Algebra.Tags
import           Noether.Lemmata.Prelude

import           Noether.Algebra.Actions.Acts
import           Noether.Algebra.Actions.Compatible
import           Noether.Algebra.Actions.Linearity

type DeriveActs_Magma op a =
  Acts_Magma (MagmaS op a)

type DeriveCompatible_Acts_Semigroup lr op act a b =
  Compatible_Acts_Semigroup
    (ActsS lr act a b)
    (SemigroupS op a)

-- | a + (b + c) = (a + b) + c
type DeriveCompatible_Associativity lr op a =
  DeriveCompatible_Acts_Semigroup lr op op a a

type DeriveActorLinearActs_Acts_Semigroup_Semigroup lr act ao a bo b =
  ActorLinear_Acts_Semigroup_Semigroup
    (ActsS lr act a b)
    (SemigroupS ao a)
    (SemigroupS bo b)

type DeriveActeeLinearActs_Acts_Semigroup_Semigroup lr act a bo b =
  ActeeLinear_Acts_Semigroup
    (ActsS lr act a b)
    (SemigroupS bo b)

-- | (a + b) * c = a * c + b * c
type DeriveActorLinearActs_LeftDistributivity lr p m a =
  DeriveActorLinearActs_Acts_Semigroup_Semigroup lr m p a p a

-- | a * (b + c) = a * b + a * c
type DeriveActeeLinearActs_RightDistributivity lr p m a =
  DeriveActeeLinearActs_Acts_Semigroup_Semigroup lr m a p a

type instance ActsS lr (op :: BinaryNumeric) Double Double =
     DeriveActs_Magma op Double

type instance CompatibleS lr (op :: BinaryNumeric) op Double Double =
     DeriveCompatible_Associativity lr op Double

type instance ActorLinearS lr Mul Add Double Add Double =
     DeriveActorLinearActs_LeftDistributivity lr Add Mul Double

type instance ActeeLinearS lr Mul Double Add Double =
     DeriveActeeLinearActs_RightDistributivity lr Add Mul Double

--------------------------------------------------------------------------------
-- Derived instances
--------------------------------------------------------------------------------

-- instance (ActsK lr a b za, SemigroupK op a zs) =>
--          CompatibleK lr op a b _


-- instance (GroupK op g zg, CompatibleK lr op g b zc) =>
--          GSetK lr op g b (Synergise '["Compatible" := zc, "Group" := zg])

--     -- Synergise
--     --    '["Acts" := za, "Semigroup/actee" := zsb, "Semigroup/actor" := zsa]

-- data family InferS (s :: Symbol) (a :: k)

-- data instance InferS "AL" z = P | Q

-- instance (ActsK lr a b za, SemigroupK ao a zsa, SemigroupK bo b zsb) =>
--          ActorLinearK lr ao a bo b (InferS "AL" (za, zsa, zsb))

-- -- type instance
-- --      Derive "ActeeLinear"
-- --        [za, zsa, zsb]
-- --      =
-- --      Synergise
-- --        '["Acts" := za, "Semigroup/actee" := zsb, "Semigroup/actor" := zsa]

-- instance ( ActsK lr a b za
--          , SemigroupK ao a zsa
--          , SemigroupK bo b zsb
--          ) => ActeeLinearK lr ao a bo b

-- -- FIXME: It seems like we need to rethink this a bit
-- type ActionFromMagma' inst (lr :: Side) op a =
--   Synergise '[ "Magma" := inst
--              , "operation" := op
--              , "side" := lr
--              ]

-- type ActionFromMagma lr op a = ActionFromMagma' (MagmaS op a) lr op a

-- instance (MagmaK op a s) =>
--          ActsK side a a (ActionFromMagma' s lr op a) where
--   actK _ _ = binaryOpK (Proxy :: Proxy op) (Proxy :: Proxy s)

-- type instance Strategy lr (a, b) "Acts" = ActsS lr a b
-- type instance Strategy (tags :: [k]) (g, b) "GSet"
--      = GSetS (Lookup' tags "side") (Lookup' tags "operation") g b

-- type instance ActsS lr Integer Integer = ActionFromMagma lr Mul Integer
-- type instance ActsS lr Double Double = ActionFromMagma lr Mul Double
-- type instance ActsS lr ComplexD ComplexD = ActionFromMagma lr Mul ComplexD
