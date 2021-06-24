{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Update.Env.HasStakeDistribution where

import           Cardano.Ledger.Update.Env.StakeDistribution (Stake,
                     StakeDistribution)
import qualified Cardano.Ledger.Update.Env.StakeDistribution as StakeDistribution

class HasStakeDistribution env k where

  stakeDistribution :: env -> StakeDistribution k

  totalStake :: env -> Stake
  totalStake = StakeDistribution.totalStake . stakeDistribution @env @k
