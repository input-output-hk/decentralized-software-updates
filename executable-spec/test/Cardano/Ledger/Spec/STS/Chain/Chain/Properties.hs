{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Spec.STS.Chain.Chain.Properties where


import           Data.Function ((&))
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import           Hedgehog (Property, forAll, property, withTests)

import           Cardano.Crypto.Hash.Short (ShortHash)

import qualified Control.State.Transition.Generator as TransitionGenerator
import qualified Control.State.Transition.Trace as Trace

import qualified Cardano.Ledger.Spec.STS.Chain.Body as Body
import           Cardano.Ledger.Spec.STS.Chain.Chain (CHAIN)
import qualified Cardano.Ledger.Spec.STS.Chain.Chain as Chain
import qualified Cardano.Ledger.Spec.STS.Chain.Transaction as Transaction
import qualified Cardano.Ledger.Spec.STS.Update as Update
import qualified Cardano.Ledger.Spec.STS.Update.Ideation as Ideation


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
                        & Chain.wrsips
                        & length
                        & fromIntegral
