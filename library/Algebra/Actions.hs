{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TemplateHaskellQuotes  #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Algebra.Actions where

import           Algebra.Basics
-- import           Data.Kind  (Type)
import           Data.Proxy
-- import           GHC.TypeLits (Symbol)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Prelude                   hiding (Monoid, negate, recip, (*),
                                            (+), (-), (/))
import qualified Prelude                   as P

-- data ActionTag = Self BinaryTag | Scalar BinaryTag

class LeftActs (tag :: BinaryTag) a b where
  leftAct' :: Proxy tag -> a -> b -> b

class RightActs (tag :: BinaryTag) a b where
  rightAct' :: Proxy tag -> b -> a -> b

class (LeftActs ltag a b, RightActs rtag a b) =>
      Acts ltag rtag a b

-- | Associative/semigroup actions

-- | The structure of 'b' is tagged by 'tag'.
class (Semigroup op a) =>
      AssocLeftActs op a tag b where
  leftAct :: Proxy tag -> Proxy op -> a -> b -> b

class (Semigroup op a) =>
      AssocRightActs op a tag b where
  rightAct :: Proxy tag -> Proxy op -> b -> a -> b

-- | FIXME: tons of distributivity implied hereafter in the actions, no?

class (AssocLeftActs op a ltag b, AssocRightActs op a rtag b) =>
      AssocActs op a ltag rtag b

class (Group op a, AssocLeftActs op a ltag b) => GSet op a ltag b

data Linear = Yes | No

type family LeftLinear (actor :: Type) (acted :: Type) (actorAdd :: BinaryTag) (actedAdd :: BinaryTag) :: Linear
