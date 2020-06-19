{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Ledger.Update.Env.HasVotingPeriodsCap where

import           Cardano.Prelude (NoUnexpectedThunks)

import           Data.Word (Word8)
import           GHC.Generics (Generic)


-- | Environments that specify a maximum number of voting periods.
class HasVotingPeriodsCap env where
  maxVotingPeriods :: env -> VotingPeriod

-- | A voting period number.
newtype VotingPeriod = VotingPeriod { unVotingPeriod :: Word8 }
  deriving stock    (Eq, Show, Generic)
  deriving newtype  (Num, Integral, Real, Enum, Ord)
  deriving anyclass (NoUnexpectedThunks)
