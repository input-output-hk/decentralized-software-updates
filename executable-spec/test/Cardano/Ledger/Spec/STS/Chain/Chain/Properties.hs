{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}


module Cardano.Ledger.Spec.STS.Chain.Chain.Properties where

import           Data.Function ((&))
import           Data.Word (Word64)
import           Data.List (foldl', any, sortBy, sum)
import qualified Test.QuickCheck as QC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Cardano.Crypto.Hash.Short (ShortHash)

import qualified Control.State.Transition.Trace as Trace
import qualified Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen

import           Ledger.Core (dom, BlockCount, unBlockCount, unSlotCount)

import           Cardano.Ledger.Spec.STS.Chain.Chain (CHAIN)
import qualified Cardano.Ledger.Spec.STS.Chain.Chain as Chain
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Cardano.Ledger.Spec.STS.Chain.Body as Body
import qualified Cardano.Ledger.Spec.STS.Chain.Transaction as Transaction
import qualified Cardano.Ledger.Spec.STS.Update as Update


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
    lastStateReveals :: Trace.Trace (CHAIN hashAlgo) -> Word64
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
  $ STS.Gen.forAllTrace @(CHAIN ShortHash) @() maxTraceLength ()
  $ \traceSample ->
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
          ( (pctUpdatePayload traceSample) >= 15
          &&
            (pctUpdatePayload traceSample) <= 25
          )
          "a reasonable pct of update payload appears in the trace "
          $

        -- Lifecycle coverage:
        -- There is at least one proposal in every phase of the lifecycle
        QC.cover 80
          (lifecycleCoverage traceSample)
          "The lifecycle of a software update is sufficently covered"
          $

        -- X% of traces should: there are SIPs that got approved
        QC.cover 25
          (traceSample `lastStateContainsTallyOutcome` Data.Approved )
          "There are approved SIPs"
          $

        -- X% of traces should: there are SIPs that got rejected
        QC.cover 25
          (traceSample `lastStateContainsTallyOutcome` Data.Rejected)
          "There are rejected SIPs"
          $

        -- X% of traces should: there are SIPs that got no quorum
        QC.cover 20
          (traceSample `lastStateContainsTallyOutcome` Data.NoQuorum)
          "There are no-quorum SIPs"
          $

        -- X% of traces should: there are SIPs that got no majority
        QC.cover 20
          (traceSample `lastStateContainsTallyOutcome` Data.NoMajority)
          "There are no-majority SIPs"
          $

        -- X% of traces should: there are SIPs that got expired
        QC.cover 10
          (traceSample `lastStateContainsTallyOutcome` Data.Expired)
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
           (stakeDistWhoOwns80PctOfStk traceSample 0.20)
           "stake distribution is skewed"
           $

        -- X% of traces should: stake distribution is uniform
        QC.cover 25
           (stakeDistWhoOwns80PctOfStk traceSample 0.80)
           "stake distribution is uniform"
           $
        QC.cover 100
          ( (length $ getSIPsInTraceFromSignals traceSample)
            ==
            (length $ getSIPsInTraceFromLastState traceSample)
          )
          "SIPs in signal equal SIPs in state"
           $
        QC.tabulate "Pct of Txs with Update Payload" [( show @Int
                                                       $ round @Float
                                                       $ pctUpdatePayload traceSample
                                                      ) ++ "%"
                                                     ]
           $
        QC.tabulate "Pct of SIP submissions in Update Payload"
                                                     [( show @Int
                                                       $ round @Float
                                                       $ pctSIPsInUpdPayload traceSample
                                                      ) ++ "%"
                                                     ]
           $
        QC.tabulate "Pct of SIP per Tally Outcome"
                                          [ ( show @Int
                                             $ round @Float
                                             $ pctSIPsTallyOutcome traceSample Data.Approved
                                            ) ++ "% Approved"
                                          , ( show @Int
                                              $ round @Float
                                              $ pctSIPsTallyOutcome traceSample Data.Rejected
                                            ) ++ "% Rejected"
                                          , ( show @Int
                                              $ round @Float
                                              $ pctSIPsTallyOutcome traceSample Data.NoQuorum
                                            ) ++ "% NoQuorum"
                                          , ( show @Int
                                              $ round @Float
                                              $ pctSIPsTallyOutcome traceSample Data.NoMajority
                                            ) ++ "% NoMajority"
                                          , ( show @Int
                                              $ round @Float
                                              $ pctSIPsTallyOutcome traceSample Data.Expired
                                            ) ++ "% Expired"
                                          ]
           $
        QC.tabulate "Pct of SIPs in revoting"
                                          [ ( show @Int
                                              $ round @Float
                                              $ pctSIPsInRevoting traceSample Data.NoQuorum
                                            ) ++ "% due to No Quorum"
                                          , ( show @Int
                                              $ round @Float
                                              $ pctSIPsInRevoting traceSample Data.NoMajority
                                            ) ++ "% due to No Majority"
                                          ]
           $ True
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
pctUpdatePayload
  :: Trace.Trace (CHAIN hashAlgo)
  -> Float
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
       then (fromIntegral txupdTot) / (fromIntegral txTot) * 100
       else 0

pctSIPsInUpdPayload :: Trace.Trace(CHAIN hashAlgo) -> Float
pctSIPsInUpdPayload tr =
  if (length $ getUpdPayload tr) /= 0
    then
      (fromIntegral $ length $ getSIPsInTraceFromLastState tr)
      /
      (fromIntegral $ length $ getUpdPayload tr)
      * 100
    else 0

-- | Percent of SIPs with the specified tally outcome
pctSIPsTallyOutcome :: Trace.Trace(CHAIN hashAlgo) -> Data.TallyOutcome -> Float
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
                     $ Data.tallyOutcomeMap vresips sDist pNoQ pNoM r_a
  in
    if (length $ getSIPsInTraceFromLastState tr) /= 0
      then
        (fromIntegral $ length $ sipsOutcome)
        /
        (fromIntegral $ length $ getSIPsInTraceFromLastState tr)
        * 100
      else 0

-- | Pct of SIPs that entered a revoting due to No Quorum or No Majority
pctSIPsInRevoting :: Trace.Trace(CHAIN hashAlgo) -> Data.TallyOutcome -> Float
pctSIPsInRevoting tr outc =
  let lastSt = Trace.lastState tr
      vresips = Chain.vresips lastSt
      sipsRevoting = case outc of
        Data.NoQuorum ->
          filter ( \(_, (Data.VotingResult {Data.rvNoQuorum})) ->
                                   if rvNoQuorum > 0
                                    then True
                                    else False
                              )
                              $ Map.toList vresips
        Data.NoMajority ->
          filter ( \(_, (Data.VotingResult {Data.rvNoMajority})) ->
                                   if rvNoMajority > 0
                                    then True
                                    else False
                              )
                              $ Map.toList vresips
        _ -> error $ "Revoting is not allowed with this "++ (show outc) ++ " tally outcome."

  in
    if (length $ Map.toList vresips) /= 0
      then
        (fromIntegral $ length $ sipsRevoting)
        /
        (fromIntegral $ length $ Map.toList vresips)
        * 100
      else 0

getSIPsInTraceFromLastState :: Trace.Trace (CHAIN hashAlgo) -> [(Data.SIP hashAlgo)]
getSIPsInTraceFromLastState tr =
  let lastSt = Trace.lastState tr
  in  map (snd)
      $ Map.toList
      $ Chain.subsips lastSt

getSIPsInTraceFromSignals :: Trace.Trace (CHAIN hashAlgo) -> [(Data.SIP hashAlgo)]
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

getUpdPayload :: Trace.Trace(CHAIN hashAlgo) -> [(Update.UpdatePayload hashAlgo)]
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


submittedSIPsExist :: Trace.Trace (CHAIN hashAlgo)  -> Bool
submittedSIPsExist tr =
  let lastSt = Trace.lastState tr
  in Chain.subsips lastSt /= Map.empty

revealedSIPsExist :: Trace.Trace (CHAIN hashAlgo)  -> Bool
revealedSIPsExist tr =
  let lastSt = Trace.lastState tr
  in Chain.sipdb lastSt /= Map.empty

sipBallotsExist :: Trace.Trace (CHAIN hashAlgo)  -> Bool
sipBallotsExist tr =
  let lastSt = Trace.lastState tr
  in Chain.ballots lastSt /= Map.empty

voteResultsExist :: Trace.Trace (CHAIN hashAlgo)  -> Bool
voteResultsExist tr =
  let lastSt = Trace.lastState tr
      vResults = Map.elems $ Chain.vresips lastSt
  in any (\vr -> Data.stakeInFavor vr /=  0
                 && Data.stakeAgainst vr /= 0
                 && Data.stakeAbstain vr /= 0
         ) vResults

-- Returns true if the last state of the inpuτ trace
-- shows that all phases in the lifecycle of a software update
-- have been covered
lifecycleCoverage
  :: Trace.Trace (CHAIN hashAlgo)
  -> Bool
lifecycleCoverage tr =
  let lastSt = Trace.lastState tr
      asips = Chain.asips lastSt /= Map.empty
      apprvsips = Chain.apprvsips lastSt /= Set.empty
  in submittedSIPsExist tr
     && revealedSIPsExist tr
     && asips
     && sipBallotsExist tr
     && voteResultsExist tr
     && apprvsips

-- Returns `True` if there is at least one SIP with a voting outcome
-- as the one in the input parameter in the last state of the trace
lastStateContainsTallyOutcome
  :: Trace.Trace (CHAIN hashAlgo)
  -> Data.TallyOutcome
  -> Bool
lastStateContainsTallyOutcome tr outc =
  let lastSt = Trace.lastState tr
      vresips = Chain.vresips lastSt
      env = Trace._traceEnv tr
      sDist = Chain.stakeDist env
      pNoQ = Chain.prvNoQuorum env
      pNoM = Chain.prvNoMajority env
      r_a = Chain.r_a env
  in any (\(_, outcome) -> outcome == outc )
     $ Map.toList $ Data.tallyOutcomeMap vresips sDist pNoQ pNoM r_a

stakeDistWhoOwns80PctOfStk
  :: Trace.Trace (CHAIN hashAlgo)
  -> Float -- ^ desired percent of stakeholders that own 80 pct of stake
  -> Bool
stakeDistWhoOwns80PctOfStk tr pctOwn =
  let env = Trace._traceEnv tr
      sDist = Chain.stakeDist env
      sDistPct = Data.stakeDistPct sDist
      stakePcts =  Map.elems sDistPct
      stakePctsDesc = sortBy (\x y -> compare y x) stakePcts
      pctOwner = take (round $ (fromIntegral $ length stakePcts) * pctOwn) stakePctsDesc
      pctOwnerTot = sum pctOwner
  in -- 20% of stakeholders hold 80% of stake
    pctOwnerTot >= 80
