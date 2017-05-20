
-- --------------------------------------------------------------------------------
-- -- Group actions
-- --------------------------------------------------------------------------------

-- type family GSetS (lr :: k1) (op :: k1) (g :: Type) (b :: Type) = (r :: Type)

-- class GSetK lr op g b s

-- type GSetC lr op g b = GSetK lr op g b (GSetS lr op g b)

-- type GSet lr op g b
--   = GSetC lr op g b
--   & Compatible lr op g b
--   & Group op g

-- type LeftGSet  op g b = GSet L op g b
-- type RightGSet op g b = GSet R op g b
