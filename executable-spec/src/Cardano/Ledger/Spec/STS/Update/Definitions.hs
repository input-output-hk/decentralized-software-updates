module Cardano.Ledger.Spec.STS.Update.Definitions where

-- | Voting threshold
-- Input is the adversary stake ratio (e.g. 0.49, or 1%3 etc.)
-- Output the percent of stake threshold (e.g., 75, 50 etc)
vThreshold :: (RealFrac a, Integral b) => a -> b
vThreshold r = round $ 50 * (r + 1)
