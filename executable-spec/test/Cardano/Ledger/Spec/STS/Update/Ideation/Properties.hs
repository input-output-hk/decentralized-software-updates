{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Spec.STS.Update.Ideation.Properties where

import           Hedgehog (Property, withTests)
import qualified Test.QuickCheck as QC

import           Cardano.Crypto.Hash.Short (ShortHash)

import qualified Control.State.Transition.Generator as TransitionGenerator

import qualified Control.State.Transition.Trace.Generator.QuickCheck as Trace.QC

import           Cardano.Ledger.Spec.STS.Update.Ideation (IDEATION)

onlyValidSignalsAreGenerated :: Property
onlyValidSignalsAreGenerated =
  withTests 300 $ TransitionGenerator.onlyValidSignalsAreGenerated @(IDEATION ShortHash) 100

qc_onlyValidSignalsAreGenerated :: QC.Property
qc_onlyValidSignalsAreGenerated
  = QC.withMaxSuccess 300
  $ Trace.QC.onlyValidSignalsAreGenerated @(IDEATION ShortHash) @() 100 ()
