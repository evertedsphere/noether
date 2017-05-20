{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Noether.Algebra.Multiple
  ( module Noether.Algebra.Multiple.Semiring
  , module Noether.Algebra.Multiple.Ring
  , module Noether.Algebra.Multiple.Strategies
  ) where

import           Noether.Lemmata.TypeFu

import           Noether.Algebra.Multiple.Ring
import           Noether.Algebra.Multiple.Semiring
import           Noether.Algebra.Multiple.Strategies
