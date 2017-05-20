module Noether.Algebra.Actions.Compatible where

import           Data.Complex

import           Noether.Lemmata.Prelude
import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Single
import           Noether.Algebra.Tags

import           Noether.Algebra.Actions.Acts


{-| A strategy-parameterized typeclass for a compatible action, where compatibility
    is defined in the group action sense.

    A compatible action satisfies
    a `act` (a' `act` b) = (a `op` a') `act` b
-}

class CompatibleK (lr :: Side) (op :: k1) (act :: k2) a b (s :: CompatibleE)

data CompatibleE = Compatible_Acts_Semigroup
  { compatible_actor           :: Type
  , compatible_action          :: ActsE
  , compatible_actor_semigroup :: SemigroupE
  }

type family CompatibleS (lr :: Side) (op :: k1) (act :: k2) (a :: Type) (b :: Type) = (r :: CompatibleE)

instance (ActsK lr act a b za, SemigroupK op a zs) =>
         CompatibleK lr op act a b (Compatible_Acts_Semigroup a za zs)
