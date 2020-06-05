{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Ledger.Spec.Classes.HasStakeDistribution where

import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import qualified Cardano.Ledger.Spec.State.StakeDistribution as StakeDistribution
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data


-- | Experts that can evaluate the SIP's.
data SIPExperts = SIPExperts

-- | Experts that can evaluate implementations.
data TechnicalExperts = TechnicalExperts

data StakePools = StakePools

class HasStakeDistribution t env p | env -> p where

  stakeDistribution :: t -> env -> StakeDistribution p

  totalStake :: t -> env -> Data.Stake
  totalStake t = StakeDistribution.totalStake . stakeDistribution t
