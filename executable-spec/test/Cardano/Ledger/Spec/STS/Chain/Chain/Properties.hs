{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}


module Cardano.Ledger.Spec.STS.Chain.Chain.Properties where

import           Control.Arrow ((&&&))
import           Data.Function ((&))
import           Data.Word (Word64)
import           Data.List (foldl', any, sortBy)
import qualified Test.QuickCheck as QC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Cardano.Crypto.Hash.Short (ShortHash)

import qualified Control.State.Transition.Trace as Trace
import qualified Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen

import           Ledger.Core (dom, (∪), BlockCount, unBlockCount, unSlotCount)

import           Cardano.Ledger.Spec.STS.Chain.Chain (CHAIN)
import qualified Cardano.Ledger.Spec.STS.Chain.Chain as Chain
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Cardano.Ledger.Spec.STS.Chain.Body as Body
import qualified Cardano.Ledger.Spec.STS.Chain.Transaction as Transaction


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

-- | Test coverage describes the test space that we are targeting for.
-- Describe the most essential characteristics of our generated traces.
--
-- TODO: this must be updated to reflect all phases in the lifecycle of
-- a software update and not just the Ideation phase.
relevantCasesAreCovered :: QC.Property
relevantCasesAreCovered
  = QC.withMaxSuccess 300
  $ STS.Gen.forAllTrace @(CHAIN ShortHash) @() maxTraceLength ()
  $ \traceSample ->
        QC.checkCoverage $
          -- traces should be long enough to allow for the stabilization
          -- of events
          QC.cover 80
            ( (fromIntegral $ Trace.traceLength traceSample)
              >=
              getMinTraceLength  (Chain.k $ Trace._traceEnv traceSample)
            )
            "Trace length is long enough to allow for the stabilization of events"
            $

          -- 80% of traces should include a 20% percent of update payload
          QC.cover 80
            ( (updatePayloadPct traceSample) >= 15
            &&
              (updatePayloadPct traceSample) <= 25
            )
            "a reasonable pct of update payload appears in the trace "
            $

          -- Lifecycle coverage:
          -- There is at least one proposal in every phase of the lifecycle
          QC.cover 50
            (lifecycleCoverage traceSample)
            "The lifecycle of a software update is sufficently covered"
            $

          -- X% of traces should: there are SIPs that got approved
          QC.cover 25
            (outcomeAny Data.Approved traceSample)
            "There are approved SIPs"
            $

          -- X% of traces should: there are SIPs that got rejected
          QC.cover 25
            (outcomeAny Data.Rejected traceSample)
            "There are rejected SIPs"
            $

          -- X% of traces should: there are SIPs that got no quorum
          QC.cover 20
            (outcomeAny Data.NoQuorum traceSample)
            "There are no-quorum SIPs"
            $

          -- X% of traces should: there are SIPs that got no majority
          QC.cover 20
            (outcomeAny Data.NoMajority traceSample)
            "There are no-majority SIPs"
            $

          -- X% of traces should: there are SIPs that got expired
          QC.cover 10
            (outcomeAny Data.Expired traceSample)
            "There are expired SIPs"
            $

          -- X% of traces should: should have submitted SIPs
          QC.cover 80
            (submittedSIPsExist traceSample)
            "Submitted SIPs exist"
            $

          -- X% of traces should: should have revealed SIPs
          QC.cover 80
            (revealedSIPsExist traceSample)
            "Revealed SIPs exist"
            $

          -- X% of traces should: should have ballots for SIPs
          QC.cover 80
            (sipBallotsExist traceSample)
            "Ballots for SIPs exist"
            $

          -- X% of traces should: should have voting results for SIPs
          QC.cover 80
            (voteResultsExist traceSample)
            "Voting results for SIPs exist"
            $

          -- TODO covers:
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

            -- X% of traces should: stake distribution is skewed
          QC.cover 25
             (stakeDistIsSkewed traceSample)
             "stake distribution is skewed"
             $

          -- X% of traces should: stake distribution is uniform
          QC.cover 25
             (stakeDistIsUniform traceSample)
             "stake distribution is uniform"
             $ qc_onlyValidSignalsAreGenerated
  where
    maxTraceLength = 100

-- Calculates a minimum required length for a trace
-- in order to allow for stabilization of events,
-- with input the k security parameter (number of Blocks)
-- submit sip - 2k blocks - reveal sip - 2k blocks - active sip -
-- voting period D blocks - 2k blocks - tally
getMinTraceLength :: BlockCount -> Word64
getMinTraceLength k =
  let kval = unBlockCount k
      minVotingDuration = unSlotCount $ Data.vpDurationToSlotCnt Data.VPMin
  in 2*kval + 2*kval + minVotingDuration + 2*kval

-- | Returns the percent of Txs with a non-empty update payload in the input Trace
updatePayloadPct
  :: Trace.Trace (CHAIN ShortHash)
  -> Float
updatePayloadPct tr =
  let -- get the total of transactions in tr
      txTot = foldl' (\tot b -> tot + txsInAblock b) 0 blocks
      blocks = (Trace.traceSignals Trace.NewestFirst tr)
      txsInAblock = \b -> length $ Body.transactions (Chain.body b)
      -- get the total of Txs with a non-empty update payload
      txupdTot = foldl' (\tot b -> tot + updtxsInAblock b) 0 blocks
      updtxsInAblock = \b -> length
                               -- Txs with a non-empty update payload
                               $ filter (\txb -> not . null
                                                  $ Transaction.update txb
                                        )
                               $ map (Transaction.body)
                               $ Body.transactions (Chain.body b)

  in (fromIntegral txupdTot) / (fromIntegral txTot) * 100

submittedSIPsExist :: Trace.Trace (CHAIN ShortHash)  -> Bool
submittedSIPsExist tr =
  let lastSt = Trace.lastState tr
  in Chain.subsips lastSt /= Map.empty

revealedSIPsExist :: Trace.Trace (CHAIN ShortHash)  -> Bool
revealedSIPsExist tr =
  let lastSt = Trace.lastState tr
  in Chain.sipdb lastSt /= Map.empty

sipBallotsExist :: Trace.Trace (CHAIN ShortHash)  -> Bool
sipBallotsExist tr =
  let lastSt = Trace.lastState tr
  in Chain.ballots lastSt /= Map.empty

voteResultsExist :: Trace.Trace (CHAIN ShortHash)  -> Bool
voteResultsExist tr =
  let lastSt = Trace.lastState tr
      vResults = Map.elems $ Chain.vresips lastSt
  in any (\vr -> Data.stakeInFavor vr /=  0
                 && Data.stakeAgainst vr /= 0
                 && Data.stakeAbstain vr /= 0
         ) vResults

-- Returns true if the last state of the inpuτ trace
-- shows that all phased in the liefecycle of a software update
-- have been covered
lifecycleCoverage
  :: Trace.Trace (CHAIN ShortHash)
  -> Bool
lifecycleCoverage tr =
  let lastSt = Trace.lastState tr
      subsips = Chain.subsips lastSt /= Map.empty
      revsips = Chain.sipdb lastSt /= Map.empty
      asips = Chain.asips lastSt /= Map.empty
      ballots = Chain.ballots lastSt /= Map.empty
      vresips = Chain.vresips lastSt /= Map.empty
      apprvsips = Chain.apprvsips lastSt /= Set.empty
  in subsips && revsips && asips && ballots && vresips && apprvsips

-- Returns `True` if there is at least one SIP with a voting outcome
-- as the one in the input parameter
outcomeAny :: Data.TallyOutcome -> Trace.Trace (CHAIN ShortHash) -> Bool
outcomeAny outc tr =
  let lastSt = Trace.lastState tr
      vresips = Chain.vresips lastSt
      env = Trace._traceEnv tr
      sDist = Chain.stakeDist env
      pNoQ = Chain.prvNoQuorum env
      pNoM = Chain.prvNoMajority env
      r_a = Chain.r_a env
  in any (\(_, outcome) -> outcome == outc )
     $ Map.toList $ Data.tallyOutcomeMap vresips sDist pNoQ pNoM r_a

stakeDistIsSkewed
  :: Trace.Trace (CHAIN ShortHash)
  -> Bool
stakeDistIsSkewed tr =
  let env = Trace._traceEnv tr
      sDist = Chain.stakeDist env
      sDistPct = Data.stakeDistPct sDist
      pcts =  Map.elems sDistPct
      pctsSortDesc = sortBy (\x y -> compare y x) pcts
      pct20 = take (round $ (fromIntegral $ length pcts) * 0.20) pctsSortDesc
      pct20Stake = foldl' (\tot s -> tot + s) 0 pct20
  in -- 20% of stakeholders hold 80% of stake
    pct20Stake >= 80

stakeDistIsUniform
  :: Trace.Trace (CHAIN ShortHash)
  -> Bool
stakeDistIsUniform tr =
  let env = Trace._traceEnv tr
      sDist = Chain.stakeDist env
      sDistPct = Data.stakeDistPct sDist
      pcts =  Map.elems sDistPct
      pctsSortDesc = sortBy (\x y -> compare y x) pcts
      pct80 = take (round $ (fromIntegral $ length pcts) * 0.80) pctsSortDesc
      pct20Stake = foldl' (\tot s -> tot + s) 0 pct80
  in -- 80% of stakeholders hold 80% of stake
    pct20Stake >= 80
