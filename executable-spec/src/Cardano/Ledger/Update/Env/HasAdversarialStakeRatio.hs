
module Cardano.Ledger.Update.Env.HasAdversarialStakeRatio where

class HasAdversarialStakeRatio e where

  adversarialStakeRatio :: e -> Float
