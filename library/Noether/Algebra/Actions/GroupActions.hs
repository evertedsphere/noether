module Noether.Algebra.Actions.GroupActions where

import Noether.Lemmata.TypeFu
import Noether.Algebra.Single
import Noether.Algebra.Tags

--------------------------------------------------------------------------------
-- Group actions
--------------------------------------------------------------------------------

type family GSetS (lr :: k1) (op :: k1) (g :: Type) (b :: Type) = (r :: Type)

class GSetK lr op g b s

type GSetC lr op g b = GSetK lr op g b (GSetS lr op g b)
