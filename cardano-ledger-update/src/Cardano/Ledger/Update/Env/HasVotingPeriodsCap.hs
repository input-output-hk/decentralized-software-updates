{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Ledger.Update.Env.HasVotingPeriodsCap where

import           Cardano.Binary (FromCBOR, ToCBOR)
import           Control.DeepSeq (NFData)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)


-- | Environments that specify a maximum number of voting periods.
class HasVotingPeriodsCap env where
  maxVotingPeriods :: env -> VotingPeriod

-- | A voting period number.
newtype VotingPeriod = VotingPeriod { unVotingPeriod :: Word8 }
  deriving stock    (Eq, Show, Generic)
  deriving newtype  (Num, Integral, Real, Enum, Ord, ToCBOR, FromCBOR)
  deriving anyclass (NoThunks, NFData, ToJSON, FromJSON)
