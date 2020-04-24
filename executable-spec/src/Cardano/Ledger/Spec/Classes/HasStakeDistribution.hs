{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Ledger.Spec.Classes.HasStakeDistribution where

import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import qualified Cardano.Ledger.Spec.State.StakeDistribution as StakeDistribution
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data

data StakePools = StakePools

class HasStakeDistribution t e p | e -> p where

  stakeDistribution :: t -> e -> StakeDistribution p

  totalStake :: t -> e -> Data.Stake
  totalStake t = StakeDistribution.totalStake . stakeDistribution t
