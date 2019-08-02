{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Ledger.Spec.STS.Sized where

import           Cardano.Prelude (HeapWords, heapWords)
import           Data.Word (Word64)

newtype WordCount = WordCount Word64
  deriving (Eq, Num, Ord, Show)

size :: HeapWords a => a -> WordCount
size = WordCount . fromIntegral . heapWords
