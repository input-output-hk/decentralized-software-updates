{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Spec.STS.Chain.Chain.Properties where

import qualified Data.Map.Strict as Map

import           Control.Arrow ((&&&))
import           Data.Function ((&))
import           Data.Typeable (typeOf)
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import           Hedgehog (Property, collect, forAll, property, withTests)
import qualified Test.QuickCheck as QC

import           Cardano.Crypto.Hash.Short (ShortHash)

import qualified Control.State.Transition.Generator as TransitionGenerator
import qualified Control.State.Transition.Trace as Trace
import qualified Control.State.Transition.Trace.Generator.QuickCheck as Trace.QC
import           Data.AbstractSize (abstractSize)

import           Ledger.Core (dom, (∪))

import           Cardano.Ledger.Spec.STS.Chain.Chain (CHAIN)
import qualified Cardano.Ledger.Spec.STS.Chain.Chain as Chain
import           Cardano.Ledger.Spec.STS.Update (UpdatePayload)

onlyValidSignalsAreGenerated :: HasCallStack => Property
onlyValidSignalsAreGenerated =
  withTests 300 $ TransitionGenerator.onlyValidSignalsAreGenerated @(CHAIN ShortHash) 100


-- tracesAreClassified :: Property
-- tracesAreClassified = withTests 300 $ property $ do
--   let (traceLength, step) = (100, 5)
--   tr <- forAll $ TransitionGenerator.trace @(CHAIN ShortHash) traceLength
--   TransitionGenerator.classifySize
--     "Reveals"
--     tr
--     lastStateReveals
--     traceLength
--     step
--   where
--     lastStateReveals :: Trace.Trace (CHAIN ShortHash) -> Word64
--     lastStateReveals tr = Trace.lastState tr
--                         & Chain.wrsips &&& Chain.asips
--                         & uncurry (∪)
--                         & dom
--                         & length
--                         & fromIntegral

qc_onlyValidSignalsAreGenerated :: QC.Property
qc_onlyValidSignalsAreGenerated
  = QC.withMaxSuccess 300
  $ Trace.QC.onlyValidSignalsAreGenerated @(CHAIN ShortHash) @() 100 ()


--------------------------------------------------------------------------------
-- Temporary, to measure differences between QC and HH.
--------------------------------------------------------------------------------

tracesAreClassified :: Property
tracesAreClassified =
  withTests 100 $ property $ do
    traceSample <- forAll $ TransitionGenerator.trace @(CHAIN ShortHash) 100
    let abstractSizes = abstractSize costs <$> Trace.traceSignals Trace.OldestFirst traceSample
        costs = Map.fromList [(typeOf (undefined :: UpdatePayload ShortHash), 1)]
    collect $ sum abstractSizes

qc_tracesAreClassified :: QC.Property
qc_tracesAreClassified =
  QC.withMaxSuccess 100
  $ Trace.QC.forAllTrace @(CHAIN ShortHash) @() 100 ()
  $ \traceSample ->
      let abstractSizes = abstractSize costs <$> Trace.traceSignals Trace.OldestFirst traceSample
          costs = Map.fromList [(typeOf (undefined :: UpdatePayload ShortHash), 1)]
      in
        QC.collect (sum abstractSizes) True
