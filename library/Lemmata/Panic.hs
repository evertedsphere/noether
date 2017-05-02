{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Trustworthy        #-}

module Lemmata.Panic (
  FatalError(..),
  panic,
) where

import           Control.Exception as X
import           Data.Text         (Text)
import           Data.Typeable     (Typeable)
import           Lemmata.Base      (Show)

-- | Uncatchable exceptions thrown and never caught.
data FatalError = FatalError { fatalErrorMessage :: Text }
  deriving (Show, Typeable)

instance Exception FatalError

panic :: Text -> a
panic a = throw (FatalError a)
