{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Spec.STS.Chain.Chain.Properties where

import           Control.Arrow ((&&&))
import           Data.Function ((&))
import           Data.Word (Word64)
import qualified Test.QuickCheck as QC

import           Cardano.Crypto.Hash.Short (ShortHash)

import qualified Control.State.Transition.Trace as Trace
import qualified Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen

import           Ledger.Core (dom, (∪))

import           Cardano.Ledger.Spec.STS.Chain.Chain (CHAIN)
import qualified Cardano.Ledger.Spec.STS.Chain.Chain as Chain


qc_onlyValidSignalsAreGenerated :: QC.Property
qc_onlyValidSignalsAreGenerated
  = QC.withMaxSuccess 1000
  $ STS.Gen.onlyValidSignalsAreGenerated @(CHAIN ShortHash) @() 25 ()

qc_traceLengthsAreClassified :: QC.Property
qc_traceLengthsAreClassified
  = QC.withMaxSuccess 100
  $ STS.Gen.traceLengthsAreClassified @(CHAIN ShortHash) 100 10 ()

qc_revealsAreClassified :: QC.Property
qc_revealsAreClassified
  = QC.withMaxSuccess 300
  $ STS.Gen.forAllTrace @(CHAIN ShortHash) @() maxTraceLength ()
  $ \traceSample ->
      STS.Gen.classifySize "Reveals" traceSample lastStateReveals maxTraceLength step
  where
    (maxTraceLength, step) = (100, 5)
    lastStateReveals :: Trace.Trace (CHAIN ShortHash) -> Word64
    lastStateReveals tr
      = Trace.lastState tr
      & Chain.wrsips &&& Chain.asips
      & uncurry (∪)
      & dom
      & length
      & fromIntegral
