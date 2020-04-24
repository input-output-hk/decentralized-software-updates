
module Cardano.Ledger.Spec.Classes.HasAdversarialStakeRatio where

class HasAdversarialStakeRatio e where

  adversarialStakeRatio :: e -> Float
