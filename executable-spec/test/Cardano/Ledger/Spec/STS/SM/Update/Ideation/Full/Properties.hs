{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Spec.STS.SM.Update.Ideation.Full.Properties where

import           Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import           Data.Set (Set, isSubsetOf)
import qualified Data.Set as Set
import           Data.Typeable (cast)
import           Test.QuickCheck

import           Cardano.Ledger.Spec.STS.Update.Definitions

import           Control.State.DataAutomata
import           Control.State.DataAutomata.Interpreter.Gen
import           Control.State.DataAutomata.Interpreter.Run
import           Control.State.DataAutomata.Interpreter.Trace

import           Cardano.Ledger.Spec.SM.Ideation
import           Cardano.Ledger.Spec.SM.Ideation.Full
import           Cardano.Ledger.Spec.SM.Vote


participants :: [Participant]
participants = Participant <$> [0 .. 10]

initialStakeDist :: StakeDistribution
initialStakeDist = StakeDistribution $ Map.fromList $ zip participants (repeat 100)

aSipId :: Word
aSipId = 0

aTauV :: Word
aTauV = vThreshold (0.3 :: Double)

prop_eventually :: ActionName -> Property
prop_eventually anActName
  = withMaxSuccess 10000
  $ expectFailure
  $ forAllTraces (Desired 300)
                 (tickerActsGen <> fullModelGens participants aSipId)
                 (Leaf (initTickerMem currentSlot) :++ fullModelInitMem aTauV initialStakeDist 3 6 currentSlot)
                 (Sync ["tick"] (Single ticker) :|| fullModel aSipId)
                 noApproval
  where
    currentSlot = 0
    noApproval trace = anActName `notElem` fmap actionName trace

prop_eventuallyApproval :: Property
prop_eventuallyApproval = prop_eventually ("approve" .@ aSipId)

prop_eventuallyRejection :: Property
prop_eventuallyRejection = prop_eventually ("reject" .@ aSipId)


prop_eventuallyN :: Set ActionName -> Property -- TODO: generalize this!
prop_eventuallyN actSet
  = withMaxSuccess 4000
  $ expectFailure
  $ forAllTraces (Desired 1000) -- NOTE: the trace length depends on the number of automata.
                 (  tickerActsGen
                 <> fullModelGens participants 0
                 <> fullModelGens participants 1
                 <> fullModelGens participants 2
                 <> fullModelGens participants 3
                 )
                 (   Leaf (initTickerMem currentSlot)
                 :++ fullModelInitMem aTauV initialStakeDist 10 30 currentSlot
                 :++ fullModelInitMem aTauV initialStakeDist 10 30 currentSlot
                 :++ fullModelInitMem aTauV initialStakeDist 10 30 currentSlot
                 :++ fullModelInitMem aTauV initialStakeDist 10 30 currentSlot
                 )
                 (   Sync ["tick"] (Single ticker)
                 :|| fullModel 0
                 :|| fullModel 1
                 :|| fullModel 2
                 :|| fullModel 3
                 )
                 noApproval
  where
    currentSlot = 0
    noApproval trace = not $ actSet `isSubsetOf` Set.fromList (fmap actionName trace)

prop_eventuallyApprovalN :: Property
prop_eventuallyApprovalN
  = prop_eventuallyN
  [ "approve" .@ (0 :: Word)
  , "approve" .@ (1 :: Word)
  , "approve" .@ (2 :: Word)
  , "approve" .@ (3 :: Word)
  ]

--------------------------------------------------------------------------------
-- Coverage test of invalid signals
--------------------------------------------------------------------------------

prop_sufficientInvalidActions :: Property
prop_sufficientInvalidActions =
  checkCoverage $ forAll invalidTraceGen coverInvalidActions
  where
    coverInvalidActions trace
      = cover 40 (0.1 < invalidPct) "More than 40% invalid actions"
      $ cover 40 (0.5 < (1 - invalidPct)) "More than 50% valid actions"
      $ True
      where
        invalidPct :: Double
        invalidPct =
          if null trace
          then 0
          else fromIntegral (length (filter isLeft trace))
               / fromIntegral (length trace)

-- TODO: we should classify approvals, rejections, and no-quorums in the invalid trace.

-- | Check that a sufficient number of approval events is generated.
prop_sufficientApprovals :: Property
prop_sufficientApprovals = prop_sufficientActions 15 ("approve" .@ aSipId)
  -- Note that we have a model in which:
  --
  -- - we need about 7 votes to meet the threshold (approval, rejection, no-quorum)
  -- - we have 10 participants
  -- - the voting period duration is 10
  -- - we have a 50 % chance of the model deciding to vote mainly for a proposal
  -- - we have a 90% of generating a vote for
  -- - participants can repeat their vote
  --
  -- So all this means that the probability of a given SIP being approved is not
  -- just about 1/3% (1/3 approval, 1/3 rejection, 1/3 no quorum).
  --
  -- If we increment the number of votes per-slot, then the chances of
  -- generating votes from different participants also increase, which makes the
  -- probability of an SIP being approved higher (and approach 1/3).
  --
  -- Also note that after a certain point, increasing the votes per-period means
  -- that we will use the trace length budget for votes only, so we won't reach
  -- the tally phase.

prop_sufficientActions
  :: Double
  -- ^ Required coverage percentage.
  -> ActionName
  -- ^ Action we're looking for.
  -> Property
prop_sufficientActions coveragePct aname =
  checkCoverage $ forAll invalidTraceGen coverAction
  where
    coverAction trace
      = cover coveragePct (trace `containsAction` aname) (show aname) True

-- | Check that a sufficient number of @"noverdict"@ events is generated.
prop_sufficientNoVerdicts :: Property
prop_sufficientNoVerdicts = prop_sufficientActions 30 ("noverdict" .@ aSipId)

prop_sufficientRejections :: Property
prop_sufficientRejections = prop_sufficientActions 15 ("reject" .@ aSipId)

-- TODO: check that we generate duplicated commits.
-- TODO: check that we generate reveals before commits.
-- TODO: check that we generate unstable reveals. [maybe] this is nice: to check this we only need to check that there are @Left "reveal"@s before the @Right "reveal"@. No need to do complicated analysis on the traces.
-- TODO: check that we generate votes after vote period ended.

invalidTraceGen :: Gen InvalidTrace
invalidTraceGen =
  automataInvalidTrace
    (tickerActsGen <> fullModelGens participants aSipId)
    (Desired 200)
    (Leaf (initTickerMem currentSlot) :++ fullModelInitMem aTauV initialStakeDist 3 20 currentSlot)
    (Sync ["tick"] (Single ticker) :|| fullModel aSipId)
    ( [ "tick"
      , "submit" .@ aSipId
      , "reveal" .@ aSipId
      , "vote" .@ aSipId
      ]
    )
  where
    currentSlot = 0


--------------------------------------------------------------------------------
-- REPL tests, for analyzing the traces.
--------------------------------------------------------------------------------

-- | Analyze the trace.
--
prop_invalidTracesAreAnalyzed :: Property
prop_invalidTracesAreAnalyzed =
  forAll invalidTraceGen collectTraceLengths
  where
    collectTraceLengths trace
      = collect (length trace)
      $ tabulate "actions" (fmap (show . actionName) trace) True

-- | Get the maximum slot number
--
--
prop_slotNumbersAreClassified :: Property
prop_slotNumbersAreClassified =
  forAll invalidTraceGen collectSlotNumbers
  where
    collectSlotNumbers trace = tabulate "Slot numbers" (show <$> slotNumbers trace) True
      where
        slotNumbers :: InvalidTrace -> [Word]
        slotNumbers [] = []
        slotNumbers (x:xs) = tryGetSlotNumber x ++ slotNumbers xs
          where
            tryGetSlotNumber (Right (CAction "tick" v)) =
              case cast v of
                Nothing   -> []
                Just slot -> [slot]
            tryGetSlotNumber _ = [] -- We aren't interested in invalid actions or other type of valid actions.
