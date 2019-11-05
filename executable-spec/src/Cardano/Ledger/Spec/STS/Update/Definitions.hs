module Cardano.Ledger.Spec.STS.Update.Definitions where

-- | Voting threshold
-- Input is the adversary stake ratio (e.g. 0.49, or 1%3 etc.)
vThreshold :: (RealFrac a, Integral b) => a -> b
vThreshold r = round $ 50 * (r + 1)
-- type VThreshold = Word8
