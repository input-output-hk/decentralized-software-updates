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

relevantCasesAreCovered :: QC.Property
relevantCasesAreCovered
  = QC.withMaxSuccess 300
  $ STS.Gen.forAllTrace @(CHAIN ShortHash) @() maxTraceLength ()
  $ \traceSample ->
      -- 80% of traces should include a [1 - 20] percent of update payload
      QC.cover 80
        ( (updatePayloadPct traceSample) >= 1
        &&
          (updatePayloadPct traceSample) <= 20
        )
        "a reasonable pct of update payload appears in the trace "
        $

      -- X% of traces should: stake distribution is skewed
      QC.cover 10
         (stakeDistIsSkewed traceSample)
         "stake distribution is skewed"
         $

      -- X% of traces should: stake distribution is uniform
      QC.cover 10
         (stakeDistIsUniform traceSample)
         "stake distribution is uniform"
         $
      -- X% of traces should: Submitted SIPs in the trace are unique

      -- X% of traces should: Submitted SIPs in the trace are not unique

      -- X% of traces should: Submitted SIPs correspond only to valid stake holders

      -- X% of traces should: Submitted SIPs correspond also to invalid stake holders

      -- X% of traces should: for every Submitted SIP there is a Reveal

      -- X% of traces should: there are Submitted SIPs that have not been Revealed yet

      -- X% of traces should: there are Revealed SIPs that have not been submitted

      -- X% of traces should: Votes correspond only to active SIPs

      -- X% of traces should: Votes correspond also to non-active SIPs
      -- (e.g., revealed, not revealed, submitted, not submitted)

      -- X% of traces should: There are active SIPs with no votes

      -- X% of traces should: there are SIPs that got approved

      -- X% of traces should: there are SIPs that got rejected

      -- X% of traces should: there are SIPs that got no-quorum

      -- X% of traces should: there are SIPs that got no-majority

      -- X% of traces should: there are SIPs that got expired

      QC.cover 40
        (Trace.traceLength traceSample <= 100)
        "there are at least as many delegation certificates as blocks"
        $ qc_onlyValidSignalsAreGenerated
  where
    maxTraceLength = 100

-- | Returns the percent of update payload in the signals of a trace
updatePayloadPct
  :: Trace.Trace sts
  -> Float
updatePayloadPct = undefined

stakeDistIsSkewed
  :: Trace.Trace sts
  -> Bool
stakeDistIsSkewed = undefined

stakeDistIsUniform
  :: Trace.Trace sts
  -> Bool
stakeDistIsUniform = undefined
