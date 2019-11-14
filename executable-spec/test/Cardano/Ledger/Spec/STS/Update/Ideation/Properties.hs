{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Spec.STS.Update.Ideation.Properties where

import qualified Test.QuickCheck as QC

import qualified Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen

import           Cardano.Ledger.Spec.STS.Update.Ideation (IDEATION)

import           Cardano.Ledger.Mock (Mock)

qc_onlyValidSignalsAreGenerated :: QC.Property
qc_onlyValidSignalsAreGenerated
  = QC.withMaxSuccess 300
  $ STS.Gen.onlyValidSignalsAreGenerated @(IDEATION Mock) @() 100 ()

qc_traceLengthsAreClassified :: QC.Property
qc_traceLengthsAreClassified
  =  QC.withMaxSuccess 300
  $ STS.Gen.traceLengthsAreClassified @(IDEATION Mock) 100 10 ()
