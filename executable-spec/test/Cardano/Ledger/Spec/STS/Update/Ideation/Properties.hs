{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Spec.STS.Update.Ideation.Properties where

import           Hedgehog (Property, withTests)

import           Cardano.Crypto.Hash.Short (ShortHash)

import qualified Control.State.Transition.Generator as TransitionGenerator

import           Cardano.Ledger.Spec.STS.Update.Ideation (IDEATION)

onlyValidSignalsAreGenerated :: Property
onlyValidSignalsAreGenerated =
  withTests 300 $ TransitionGenerator.onlyValidSignalsAreGenerated @(IDEATION ShortHash) 100
