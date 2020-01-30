-- | Transaction-bytes per-second.

module Cardano.Ledger.Benchmarks.Update.WorstCaseAnalysis.Units.TBPS
  ( TBPS
  , tbps
  , unTBPS
  , transmissionTime
  , capacity
  )
where

import           Prelude hiding ((/))

import           Numeric.Natural (Natural)

import           Cardano.Ledger.Benchmarks.Update.WorstCaseAnalysis.Arithmetic
                     ((/))
import           Cardano.Ledger.Benchmarks.Update.WorstCaseAnalysis.Units

-- | Transaction bytes per-second.
newtype TBPS = TBPS { unTBPS :: Natural }
  deriving (Show, Eq, Ord)

-- | Calculate the transaction bytes per-second by dividing the number of bytes
-- by the given duration. The result is rounded down.
--
tbps :: NumberOfBytes -> Seconds -> TBPS
tbps (NumberOfBytes n) (Seconds s) =
  TBPS $ floor $ fromIntegral n / s

instance Multipliable TBPS where
  times (TBPS m) n = TBPS $ m * fromIntegral n

-- | Calculate the time it takes to transmit the given number of bytes, given a
-- (system) TBPS.
transmissionTime :: NumberOfBytes -> TBPS -> Seconds
transmissionTime (NumberOfBytes n) (TBPS m)
  = Seconds
  $ ceiling
  $ n / m

-- | Calculate the number of bytes that can be processed given a system TBPS and
-- the period over which bytes are transmitted.
capacity :: TBPS -> Seconds -> NumberOfBytes
capacity (TBPS m) (Seconds s)
  = NumberOfBytes
  $ m * s
