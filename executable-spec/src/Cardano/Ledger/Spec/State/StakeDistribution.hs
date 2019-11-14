{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.StakeDistribution where

import           Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Word (Word8)

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Indexed (Indexed)


newtype StakeDistribution p = StakeDistribution (Map Core.VKey Data.Stake)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Core.Relation, Semigroup, Monoid, Indexed)

-- | Returns the total stake from a stake distribution.
totalStake :: StakeDistribution p -> Data.Stake
totalStake (StakeDistribution stakeMap) =
  Map.foldr' (+) (Data.Stake 0) stakeMap

-- | Returns a map showing the percent @[0,100]@ of stake ownership of each
-- stakeholder.
stakeDistPct
  :: StakeDistribution p
  -> Map Core.VKey Word8
stakeDistPct stakeDistribution@(StakeDistribution stakeMap) =
  Map.map (\st -> Data.stakePercentRound st $ totalStake stakeDistribution) stakeMap
