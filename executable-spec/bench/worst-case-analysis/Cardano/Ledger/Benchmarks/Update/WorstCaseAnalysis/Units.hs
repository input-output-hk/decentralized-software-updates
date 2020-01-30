{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Ledger.Benchmarks.Update.WorstCaseAnalysis.Units where

import           Numeric.Natural (Natural)

newtype NumberOfBytes = NumberOfBytes { unNumberOfBytes ::  Natural}
  deriving (Num, Show, Eq, Real, Integral, Enum, Ord)

newtype NumberOfSlots = NumberOfSlots Natural
  deriving (Num, Show, Eq, Real, Integral, Enum, Ord)

newtype Seconds = Seconds { unSeconds :: Natural }
  deriving (Num, Show, Eq, Real, Integral, Enum, Ord)

-- | Magnitudes that can be multipled.
class Multipliable m where
  times :: Integral n => m -> n -> m

instance Multipliable NumberOfBytes where
  times (NumberOfBytes m) n = NumberOfBytes $ m * fromIntegral n
