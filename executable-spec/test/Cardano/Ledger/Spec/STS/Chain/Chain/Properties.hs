{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Spec.STS.Chain.Chain.Properties where

import           Control.Arrow ((&&&))
import           Data.Function ((&))
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import           Hedgehog (Property, forAll, property, withTests)
import qualified Test.QuickCheck as QC

import           Cardano.Crypto.Hash.Short (ShortHash)

import qualified Control.State.Transition.Generator as TransitionGenerator
import qualified Control.State.Transition.Trace as Trace
import qualified Control.State.Transition.Trace.Generator.QuickCheck as Trace.QC

import           Ledger.Core (dom, (∪))

import           Cardano.Ledger.Spec.STS.Chain.Chain (CHAIN)
import qualified Cardano.Ledger.Spec.STS.Chain.Chain as Chain


onlyValidSignalsAreGenerated :: HasCallStack => Property
onlyValidSignalsAreGenerated =
  withTests 300 $ TransitionGenerator.onlyValidSignalsAreGenerated @(CHAIN ShortHash) 100


tracesAreClassified :: Property
tracesAreClassified = withTests 300 $ property $ do
  let (traceLength, step) = (100, 5)
  tr <- forAll $ TransitionGenerator.trace @(CHAIN ShortHash) traceLength
  TransitionGenerator.classifySize
    "Reveals"
    tr
    lastStateReveals
    traceLength
    step
  where
    lastStateReveals :: Trace.Trace (CHAIN ShortHash) -> Word64
    lastStateReveals tr = Trace.lastState tr
                        & Chain.wrsips &&& Chain.asips
                        & uncurry (∪)
                        & dom
                        & length
                        & fromIntegral

qc_onlyValidSignalsAreGenerated :: QC.Property
qc_onlyValidSignalsAreGenerated
  = QC.withMaxSuccess 300
  $ Trace.QC.onlyValidSignalsAreGenerated @(CHAIN ShortHash) @() 100 ()
