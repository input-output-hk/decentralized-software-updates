{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Spec.STS.Update.Ideation.Properties where

import           Hedgehog (Property, collect, forAll, property, withTests)
import qualified Test.QuickCheck as QC

import           Cardano.Crypto.Hash.Short (ShortHash)

import qualified Control.State.Transition.Generator as TransitionGenerator
import qualified Control.State.Transition.Trace as Trace

import qualified Control.State.Transition.Trace.Generator.QuickCheck as Trace.QC

import           Cardano.Ledger.Spec.STS.Update.Ideation (IDEATION)

onlyValidSignalsAreGenerated :: Property
onlyValidSignalsAreGenerated =
  withTests 300 $ TransitionGenerator.onlyValidSignalsAreGenerated @(IDEATION ShortHash) 100

tracesAreClassified :: Property
tracesAreClassified =
  withTests 100 $ property $ do
    traceSample <- forAll $ TransitionGenerator.trace @(IDEATION ShortHash) 100
    collect $ Trace.traceLength traceSample

qc_onlyValidSignalsAreGenerated :: QC.Property
qc_onlyValidSignalsAreGenerated
  = QC.withMaxSuccess 300
  $ Trace.QC.onlyValidSignalsAreGenerated @(IDEATION ShortHash) @() 100 ()

qc_tracesAreClassified :: QC.Property
qc_tracesAreClassified =
  QC.withMaxSuccess 100
  $ Trace.QC.forAllTrace @(IDEATION ShortHash) @() 100 ()
  $ \traceSample ->
    QC.collect (Trace.traceLength traceSample) True
