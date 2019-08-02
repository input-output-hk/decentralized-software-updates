{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Spec.STS.Chain.Chain.Properties where


import           Hedgehog (Property, withTests)

import qualified Control.State.Transition.Generator as TransitionGenerator

import           Cardano.Ledger.Spec.STS.Chain.Chain (CHAIN)

onlyValidSignalsAreGenerated :: Property
onlyValidSignalsAreGenerated =
  withTests 100 $ TransitionGenerator.onlyValidSignalsAreGenerated @CHAIN 1000
