{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Ledger.Spec.STS.Sized where

import qualified Data.Map as Map
import           Data.Typeable (TypeRep)
import           Data.Word (Word64)

import           Data.AbstractSize (AccountingMap, HasTypeReps, abstractSize)


-- | Size of a value. The interpretation of this value (e.g. bytes, words,
-- number of components) depend of the 'Sized' instance.
--
newtype Size = Size Word64
  deriving (Eq, Num, Ord, Show)


-- | A Sized value has a 'cost' function to determine the size each of its
-- components take.
--
-- The size each component take depends on the interpretation of the 'Size' of a
-- value: it might be bytes, but it might also be its abstract size.
--
class HasTypeReps a => Sized a where
  costs :: a -> AccountingMap
  costs = Map.fromList . costsList

  costsList :: a -> [(TypeRep, Int)]

size :: Sized a => a -> Size
size a = fromIntegral $ abstractSize (costs a) a
