module Cardano.Ledger.Spec.STS.Update.Definitions where

import           Cardano.Ledger.Spec.STS.Update.Data (Stake)

-- | Voting threshold.
--
-- Returns the percent of stake threshold (e.g., 75, 50 etc).
vThreshold
  :: (RealFrac a, Integral b)
  => a
  -- ^ adversary stake ratio (e.g. 0.49, or 1%3 etc.)
  -> b
vThreshold r_a = round $ 50 * (r_a + 1)

-- | Given a total stake and the adversary stake ratio, compute the how much
-- stake of this total stake is needed to meet the voting threshold.
--
-- This definition is based on 'vThreshold'. We have that the total stake needed
-- for a given @r_a@ equals:
--
-- > vThreshold r_a / 100 * totalStake
-- > = { def. vThreshold }
-- > 50 * (r_a + 1) / 100 * totalStake
-- > = { algebra }
-- > 1/2 * (r_a + 1) * totalStake
--
stakeThreshold
  :: (RealFrac a)
  => a
  -> Stake
  -> Stake
stakeThreshold r_a totalStake =
  round $ 1/2 * (r_a + 1) * fromIntegral totalStake
