{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Spec.STS.Chain.Chain.Properties where

import           Data.Foldable (toList)
import           Data.Function ((&))
import           Data.List (any, foldl', sortBy, sum)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Word (Word64, Word8)
import qualified Test.QuickCheck as QC

import qualified Control.State.Transition.Trace as Trace
import qualified Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen

import           Ledger.Core (BlockCount, dom, range, size, unBlockCount,
                     unSlotCount)

import           Cardano.Ledger.Spec.State.SIPsVoteResults
                     (SIPsVoteResults (SIPsVoteResults))
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution,
                     stakeDistPct)
import qualified Cardano.Ledger.Spec.STS.Chain.Body as Body
import           Cardano.Ledger.Spec.STS.Chain.Chain (CHAIN)
import qualified Cardano.Ledger.Spec.STS.Chain.Chain as Chain
import qualified Cardano.Ledger.Spec.STS.Chain.Transaction as Transaction
import qualified Cardano.Ledger.Spec.STS.Update as Update
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Cardano.Ledger.Spec.STS.Update.Tallysip as Tallysip

import           Cardano.Ledger.Test.Mock (Mock)

qc_onlyValidSignalsAreGenerated :: QC.Property
qc_onlyValidSignalsAreGenerated
  = QC.withMaxSuccess 1000
  $ STS.Gen.onlyValidSignalsAreGenerated @(CHAIN Mock) @() 25 ()

qc_traceLengthsAreClassified :: QC.Property
qc_traceLengthsAreClassified
  = QC.withMaxSuccess 100
  $ STS.Gen.traceLengthsAreClassified @(CHAIN Mock) 100 10 ()

qc_revealsAreClassified :: QC.Property
qc_revealsAreClassified
  = QC.withMaxSuccess 300
  $ STS.Gen.forAllTrace @(CHAIN Mock) @() maxTraceLength ()
  $ \traceSample ->
      STS.Gen.classifySize "Reveals" traceSample lastStateReveals maxTraceLength step
  where
    (maxTraceLength, step) = (100, 5)
    lastStateReveals :: Trace.Trace (CHAIN Mock) -> Word64
    lastStateReveals tr
      = Trace.lastState tr
      & Chain.sipdb
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
  $ STS.Gen.forAllTrace @(CHAIN Mock) @() maxTraceLength ()
  $ \traceSample ->
        (QC..&&.)
          (STS.Gen.classifyTraceLength maxTraceLength 10 traceSample)
        $
        -- traces should be long enough to allow for the stabilization
        -- of events
        QC.cover 70
          ( (fromIntegral $ Trace.traceLength traceSample)
            >=
            getMinTraceLength  (Chain.k $ Trace._traceEnv traceSample)
          )
          "Trace length is long enough to allow for the stabilization of events"
          $
--        QC.collect (pctUpdatePayload traceSample) $
        -- 50% of traces should include a 20% percent of update payload
        QC.cover 50
          ( (pctUpdatePayload traceSample) >= 10
           &&
            (pctUpdatePayload traceSample) <= 30
          )
          "a reasonable pct of update payload appears in the trace "
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
        QC.cover 60
          (voteResultsExist traceSample)
          "Voting results for SIPs exist"
          $


        -- Lifecycle coverage:
        -- There is at least one proposal in every phase of the lifecycle
        QC.cover 60
          (lifecycleCoverage traceSample)
          "The lifecycle of a software update is sufficently covered"
          $

        -- X% of traces should: there are SIPs that got approved
        QC.cover 50
          (traceSample `lastStateContainsTallyOutcome` Data.Approved )
          "There are approved SIPs"
          $

        -- X% of traces should: there are SIPs that got rejected
        QC.cover 2
          (traceSample `lastStateContainsTallyOutcome` Data.Rejected)
          "There are rejected SIPs"
          $

        -- X% of traces should: there are SIPs that got no quorum
        QC.cover 2
          (traceSample `lastStateContainsTallyOutcome` Data.NoQuorum)
          "There are no-quorum SIPs"
          $
        -- QC.collect ( let SIPsVoteResults (vresmap) = Chain.vresips $ Trace.lastState traceSample
        --              in sum $ map(\vr -> Data.stakeInFavor vr) $ map (snd) $ Map.toList vresmap
        --            )
        --   $
        -- QC.collect ( let SIPsVoteResults (vresmap) = Chain.vresips $ Trace.lastState traceSample
        --              in sum $ map(\vr -> Data.stakeAgainst vr) $ map (snd) $ Map.toList vresmap
        --            )
        --   $
        -- QC.collect ( let SIPsVoteResults (vresmap) = Chain.vresips $ Trace.lastState traceSample
        --              in sum $ map(\vr -> Data.stakeAbstain vr) $ map (snd) $ Map.toList vresmap
        --            )
        --   $
        -- X% of traces should: there are SIPs that got no majority
        QC.cover 10
          (traceSample `lastStateContainsTallyOutcome` Data.NoMajority)
          "There are no-majority SIPs"
          $

        -- X% of traces should: there are SIPs that got expired
        QC.cover 10
          (traceSample `lastStateContainsTallyOutcome` Data.Expired)
          "There are expired SIPs"
          $ True


        -- -- TODO covers:
        --   -- X% of traces should: Submitted SIPs in the trace are unique

        --   -- X% of traces should: Submitted SIPs in the trace are not unique

        --   -- X% of traces should: Submitted SIPs correspond only to valid stake holders

        --   -- X% of traces should: Submitted SIPs correspond also to invalid stake holders

        --   -- X% of traces should: for every Submitted SIP there is a Reveal

        --   -- X% of traces should: there are Submitted SIPs that have not been Revealed yet

        --   -- X% of traces should: there are Revealed SIPs that have not been submitted

        --   -- X% of traces should: Votes correspond only to active SIPs

        --   -- X% of traces should: Votes correspond also to non-active SIPs
        --   -- (e.g., revealed, not revealed, submitted, not submitted)

        --   -- X% of traces should: There are active SIPs with no votes

        --   -- X% of traces should: stake distribution is skewed
        -- QC.cover 25
        --    (stakeDistWhoOwns80PctOfStk traceSample 0.20)
        --    "stake distribution is skewed"
        --    $

        -- -- X% of traces should: stake distribution is uniform
        -- QC.cover 25
        --    (stakeDistWhoOwns80PctOfStk traceSample 0.80)
        --    "stake distribution is uniform"
        --    $
        -- QC.cover 100
        --   ( (length $ getSIPsInTraceFromSignals traceSample)
        --     ==
        --     (length $ getSIPsInTraceFromLastState traceSample)
        --   )
        --   "SIPs in signal equal SIPs in state"
        --    $
        -- QC.tabulate "Pct of Txs with Update Payload" [( show @Int
        --                                                $ round @Float
        --                                                $ pctUpdatePayload traceSample
        --                                               ) ++ "%"
        --                                              ]
        --    $
        -- QC.tabulate "Pct of SIP submissions in Update Payload"
        --                                              [( show @Int
        --                                                $ round @Float
        --                                                $ pctSIPsInUpdPayload traceSample
        --                                               ) ++ "%"
        --                                              ]
        --    $
        -- QC.tabulate "Pct of SIP per Tally Outcome"
        --                                   [ ( show @Int
        --                                      $ round @Float
        --                                      $ pctSIPsTallyOutcome traceSample Data.Approved
        --                                     ) ++ "% Approved"
        --                                   , ( show @Int
        --                                       $ round @Float
        --                                       $ pctSIPsTallyOutcome traceSample Data.Rejected
        --                                     ) ++ "% Rejected"
        --                                   , ( show @Int
        --                                       $ round @Float
        --                                       $ pctSIPsTallyOutcome traceSample Data.NoQuorum
        --                                     ) ++ "% NoQuorum"
        --                                   , ( show @Int
        --                                       $ round @Float
        --                                       $ pctSIPsTallyOutcome traceSample Data.NoMajority
        --                                     ) ++ "% NoMajority"
        --                                   , ( show @Int
        --                                       $ round @Float
        --                                       $ pctSIPsTallyOutcome traceSample Data.Expired
        --                                     ) ++ "% Expired"
        --                                   ]
        --    $
        -- QC.tabulate "Pct of SIPs in revoting"
        --                                   [ ( show @Int
        --                                       $ round @Float
        --                                       $ pctSIPsInRevoting traceSample Data.NoQuorum
        --                                     ) ++ "% due to No Quorum"
        --                                   , ( show @Int
        --                                       $ round @Float
        --                                       $ pctSIPsInRevoting traceSample Data.NoMajority
        --                                     ) ++ "% due to No Majority"
        --                                   ]
        --    $ True
  where
    maxTraceLength = 200

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
pctUpdatePayload
  :: Trace.Trace (CHAIN Mock)
  -> Int
pctUpdatePayload tr =
  let -- get the total of transactions in tr
      txTot = foldl' (\tot b -> tot + txsInAblock b) 0 blocks
      blocks = Trace.traceSignals Trace.NewestFirst tr
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

  in if txTot > 0
       then round @Float $ (fromIntegral txupdTot) / (fromIntegral txTot) * 100
       else 0

pctSIPsInUpdPayload :: Trace.Trace(CHAIN Mock) -> Float
pctSIPsInUpdPayload tr =
  if (length $ getUpdPayload tr) /= 0
    then
      (fromIntegral $ length $ getSIPsInTraceFromLastState tr)
      /
      (fromIntegral $ length $ getUpdPayload tr)
      * 100
    else 0

-- | Percent of SIPs with the specified tally outcome
pctSIPsTallyOutcome :: Trace.Trace(CHAIN Mock) -> Data.TallyOutcome -> Float
pctSIPsTallyOutcome tr outc =
  let lastSt = Trace.lastState tr
      vresips = Chain.vresips lastSt
      env = Trace._traceEnv tr
      sDist = Chain.stakeDist env
      pNoQ = Chain.prvNoQuorum env
      pNoM = Chain.prvNoMajority env
      r_a = Chain.r_a env
      sipsOutcome =  map (fst)
                     $ filter (\(_, outcome) -> outcome == outc )
                     $ Map.toList
                     $ tallyOutcomeMap vresips sDist pNoQ pNoM r_a
  in
    if (length $ getSIPsInTraceFromLastState tr) /= 0
      then
        (fromIntegral $ length $ sipsOutcome)
        /
        (fromIntegral $ length $ getSIPsInTraceFromLastState tr)
        * 100
      else 0


-- | Return a SIP-hash to TallyOutcome map
tallyOutcomeMap
  :: SIPsVoteResults p
  -> StakeDistribution p
  -> Word8  -- ^ max number of revoting for No Quorum
  -> Word8  -- ^ max number of revoting for No Majority
  -> Float  -- ^ adversary stake ratio
  -> Map (Data.SIPHash p) Data.TallyOutcome
tallyOutcomeMap (SIPsVoteResults vresips) sDist pNoQ pNoM r_a =
  Map.map (\vr -> Tallysip.tallyOutcome vr sDist pNoQ pNoM r_a) vresips


-- | Pct of SIPs that entered a revoting due to No Quorum or No Majority
pctSIPsInRevoting :: Trace.Trace (CHAIN Mock) -> Data.TallyOutcome -> Float
pctSIPsInRevoting tr outc =
  let lastSt = Trace.lastState tr
      vresips = Chain.vresips lastSt
      sipsRevoting = case outc of
        Data.NoQuorum ->
          Set.filter ((0 < ). Data.rvNoQuorum) $ range vresips
        Data.NoMajority ->
          Set.filter ((0 < ). Data.rvNoMajority) $ range vresips
        _ -> error $ "Revoting is not allowed with this "++ (show outc) ++ " tally outcome."

  in
    if (size vresips :: Int) /= 0
      then
        (fromIntegral $ length $ sipsRevoting)
        /
        (fromIntegral $ (size vresips :: Int))
        * 100
      else 0

getSIPsInTraceFromLastState :: Trace.Trace (CHAIN Mock) -> [(Data.SIP Mock)]
getSIPsInTraceFromLastState tr
  = Trace.lastState tr
  & Chain.subsips
  & range
  & toList

getSIPsInTraceFromSignals :: Trace.Trace (CHAIN Mock) -> [(Data.SIP Mock)]
getSIPsInTraceFromSignals tr =
  let sips = foldl' (\tot b ->  tot ++ (sipsInABlock b)) [] blocks
      blocks = Trace.traceSignals Trace.NewestFirst tr
      sipsInABlock = \b -> map (\updPld -> case updPld of
                                             Update.Ideation (Data.Submit _ sip)
                                               -> sip
                                             _ -> error $
                                                   "getSIPsInTraceFromSignals:" ++
                                                   " The execution path cannot " ++
                                                   " reach this point!"

                               )
                           $ filter ( -- get sips only from upd payload
                                      \updPld -> case updPld of
                                        Update.Ideation
                                          (Data.Submit _ _) -> True
                                        _ -> False
                                    )
                           $ concat
                           $ map (Transaction.update)
                             -- Txs with a non-empty update payload
                           $ filter (\txb -> not . null
                                              $ Transaction.update txb
                                    )
                           $ map (Transaction.body)
                           $ Body.transactions (Chain.body b)
  in sips

getUpdPayload :: Trace.Trace(CHAIN Mock) -> [(Update.UpdatePayload Mock)]
getUpdPayload tr =
  let upds = foldl' (\tot b ->  tot ++ (updsInABlock b)) [] blocks
      blocks = Trace.traceSignals Trace.NewestFirst tr
      updsInABlock = \b -> concat
                           $ map (Transaction.update)
                             -- Txs with a non-empty update payload
                           $ filter (\txb -> not . null
                                              $ Transaction.update txb
                                    )
                           $ map (Transaction.body)
                           $ Body.transactions (Chain.body b)
  in upds


submittedSIPsExist :: Trace.Trace (CHAIN Mock)  -> Bool
submittedSIPsExist tr =
  let lastSt = Trace.lastState tr
  in Chain.subsips lastSt /= mempty

revealedSIPsExist :: Trace.Trace (CHAIN Mock)  -> Bool
revealedSIPsExist tr =
  let lastSt = Trace.lastState tr
  in Chain.sipdb lastSt /= mempty

sipBallotsExist :: Trace.Trace (CHAIN Mock)  -> Bool
sipBallotsExist tr =
  let lastSt = Trace.lastState tr
  in Chain.ballots lastSt /= mempty

voteResultsExist :: Trace.Trace (CHAIN Mock)  -> Bool
voteResultsExist tr =
  let lastSt = Trace.lastState tr
      vResults = range $ Chain.vresips lastSt
  in any (\vr -> (  Data.stakeInFavor vr >  0
                 || Data.stakeAgainst vr > 0
                 || Data.stakeAbstain vr > 0
                 )
                 ||
                 (  Data.stakeInFavor vr == 0
                 && Data.stakeAgainst vr == 0
                 && Data.stakeAbstain vr == 0
                 && (  Data.rvNoQuorum vr > 0
                    || Data.rvNoMajority vr > 0
                    )
                 )
         ) vResults

-- Returns true if the last state of the inpuτ trace
-- shows that all phases in the lifecycle of a software update
-- have been covered
lifecycleCoverage
  :: Trace.Trace (CHAIN Mock)
  -> Bool
lifecycleCoverage tr =
  let lastSt = Trace.lastState tr
      asips = Chain.asips lastSt /= mempty
      apprvsips = Chain.apprvsips lastSt /= mempty
  in submittedSIPsExist tr
     && revealedSIPsExist tr
     && asips
     && sipBallotsExist tr
     && voteResultsExist tr
     && apprvsips

-- Returns `True` if there is at least one SIP with a voting outcome
-- as the one in the input parameter in the last state of the trace
lastStateContainsTallyOutcome
  :: Trace.Trace (CHAIN Mock)
  -> Data.TallyOutcome
  -> Bool
lastStateContainsTallyOutcome tr outc =
  let lastSt = Trace.lastState tr
      vresips@(SIPsVoteResults (vresmap)) = Chain.vresips lastSt
      env = Trace._traceEnv tr
      sDist = Chain.stakeDist env
      pNoQ = Chain.prvNoQuorum env
      pNoM = Chain.prvNoMajority env
      r_a = Chain.r_a env
  in if outc /= Data.NoQuorum && outc /= Data.NoMajority
       then
         any (\(_, outcome) -> outcome == outc )
         $ Map.toList $ tallyOutcomeMap vresips sDist pNoQ pNoM r_a
       else
          if outc == Data.NoQuorum
            then
               -- NoQuorum existence must be indicated NOT by calculating the outcome
               -- because due to revoting the abstain stake is constantly zeroed
               -- but by the existence of a noquorum revoting counter > 0
               any (\rvnoq -> rvnoq > 0)
                   $ map(\vr -> Data.rvNoQuorum vr)
                   $ map (snd)
                   $ Map.toList vresmap
             else -- NoMajority
               -- NoMajority existence must be indicated NOT by calculating the outcome
               -- because due to revoting the voting stakes are constantly zeroed
               -- but by the existence of a nomajority revoting counter > 0
               any (\rvnom -> rvnom > 0)
                   $ map(\vr -> Data.rvNoMajority vr)
                   $ map (snd)
                   $ Map.toList vresmap

stakeDistWhoOwns80PctOfStk
  :: Trace.Trace (CHAIN Mock)
  -> Float -- ^ desired percent of stakeholders that own 80 pct of stake
  -> Bool
stakeDistWhoOwns80PctOfStk tr pctOwn =
  let env = Trace._traceEnv tr
      sDist = Chain.stakeDist env
      sDistPct = stakeDistPct sDist
      stakePcts =  Map.elems sDistPct
      stakePctsDesc = sortBy (\x y -> compare y x) stakePcts
      pctOwner = take (round $ (fromIntegral $ length stakePcts) * pctOwn) stakePctsDesc
      pctOwnerTot = sum pctOwner
  in -- 20% of stakeholders hold 80% of stake
    pctOwnerTot >= 80
