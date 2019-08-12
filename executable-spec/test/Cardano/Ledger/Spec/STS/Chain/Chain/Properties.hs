{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Spec.STS.Chain.Chain.Properties where


import           Hedgehog (Property, withTests)
import           GHC.Stack (HasCallStack)

import qualified Control.State.Transition.Generator as TransitionGenerator

import           Cardano.Ledger.Spec.STS.Chain.Chain (CHAIN)

onlyValidSignalsAreGenerated :: HasCallStack => Property
onlyValidSignalsAreGenerated =
  withTests 300 $ TransitionGenerator.onlyValidSignalsAreGenerated @CHAIN 100
