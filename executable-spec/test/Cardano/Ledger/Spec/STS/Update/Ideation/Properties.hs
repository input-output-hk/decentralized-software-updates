{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Spec.STS.Update.Ideation.Properties where

import           Hedgehog (Property, withTests)

import qualified Control.State.Transition.Generator as TransitionGenerator

import           Cardano.Ledger.Spec.STS.Update.Ideation (IDEATION)

onlyValidSignalsAreGenerated :: Property
onlyValidSignalsAreGenerated =
  withTests 300 $ TransitionGenerator.onlyValidSignalsAreGenerated @IDEATION 100
