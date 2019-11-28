{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Spec.STS.SM.Update.Ideation.Vote.Properties where

import           Control.Monad.Except (Except)
import qualified Data.Map as Map
import           Test.QuickCheck

import           Control.State.DataAutomata
import           Control.State.DataAutomata.Interpreter.Gen hiding (Error)
import           Control.State.DataAutomata.Interpreter.Memory hiding (Error)
import           Control.State.DataAutomata.Interpreter.Run
import           Control.State.DataAutomata.Interpreter.Trace

import           Cardano.Ledger.Spec.STS.Update.Definitions (vThreshold)

import           Cardano.Ledger.Spec.SM.Ideation
import           Cardano.Ledger.Spec.SM.Vote

--------------------------------------------------------------------------------
-- Global tests parameters (temporary till we can generate these)
--------------------------------------------------------------------------------

-- TODO: we will want to generate this.
vks :: [Participant]
vks = Participant <$> [0 .. 10]

-- TODO: this will have to be generated.
initialStakeDist :: StakeDistribution
initialStakeDist = StakeDistribution $ Map.fromList $ zip vks (repeat 100)

-- TODO: generate this. The adversarial stake ratio should be between 0 and 0.5.
-- We might want to test what happens with higher adversarial ratio.
aTauV :: Word
aTauV = vThreshold (0.3 :: Double)

--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

voteSMEx0 :: Except Error (Memory, State)
voteSMEx0 = run (initVoteTallyMem aTauV initialStakeDist)
                (voteTally autId)
                [ CAction ("vote" .@ autId) $ Vote (vks !! 0) Against
                , CAction ("vote" .@ autId) $ Vote (vks !! 1) Against
                , CAction ("vote" .@ autId) $ Vote (vks !! 2) Against
                , CAction ("vote" .@ autId) $ Vote (vks !! 3) Against
                , CAction ("vote" .@ autId) $ Vote (vks !! 4) Against
                , CAction ("vote" .@ autId) $ Vote (vks !! 5) Against
                , CAction ("vote" .@ autId) $ Vote (vks !! 6) Against
                , CAction ("vote" .@ autId) $ Vote (vks !! 7) Against
                , CAction ("tally" .@ autId) ()
                , CAction ("reject" .@ autId) ()
                -- Replacing the action above by the action below will lead to
                -- deadlock, as the votes can't be approved:
                --
                -- >  CAction "approve"  (15 :: Word)
                ]

example0 :: Property
example0 = finalState voteSMEx0 === "Rejected"

voteSMEx1 :: Except Error (Memory, State)
voteSMEx1 = run (initVoteTallyMem aTauV initialStakeDist)
                (voteTally autId)
                [ CAction ("vote" .@ autId) $ Vote (vks !! 0) For
                , CAction ("vote" .@ autId) $ Vote (vks !! 1) For
                , CAction ("vote" .@ autId) $ Vote (vks !! 2) For
                , CAction ("vote" .@ autId) $ Vote (vks !! 3) For
                , CAction ("vote" .@ autId) $ Vote (vks !! 4) For
                , CAction ("vote" .@ autId) $ Vote (vks !! 5) For
                , CAction ("vote" .@ autId) $ Vote (vks !! 6) For
                , CAction ("vote" .@ autId) $ Vote (vks !! 7) For
                , CAction ("tally" .@ autId) ()
                , CAction ("approve" .@ autId) ()
                ]

example1 :: Property
example1 = finalState voteSMEx1 === "Approved"

genVote :: Gen Vote
genVote = do
  who <- elements vks
  aDecision <- elements [For, Against, Abstain]
  pure $ Vote { vk = who
              , decision = aDecision
              }

genStakeUpdate :: Gen StakeUpdate
genStakeUpdate = do
  someParticipant <- elements vks
  anotherParticipant <- elements vks
  somePercentage <- choose (0.0, 1.0)
  pure $ StakeUpdate { fromVk = someParticipant
                     , toVk = anotherParticipant
                     , percentToTransfer = somePercentage
                     }

genVoteTrace :: Gen [CAction]
genVoteTrace =
  genAutomatonTrace
    [ ("stakeUpdate", Cell <$> genStakeUpdate)
    , ("vote", Cell <$> genVote)
    , ("tally", Cell <$> pure autId)
    ]
    (initVoteTallyMem aTauV initialStakeDist)
    (voteTally autId)

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

-- | Property that shows that a tally signal can be eventually generated.
prop_noTally :: Property
prop_noTally
  = expectFailure
  $ forAllTraces (Desired 50)
                 (tickerActsGen) -- <> revealsGen autId)
                 (Leaf (initTickerMem currentSlot) :++ Leaf (initActivationMem 3 6))
                 (Sync ["tick"] (Single ticker) :|| Sync ["tick"] (Single $ activation autId))
                 noTally
  where
    currentSlot = 0
    noTally trace = ("tally".@ autId) `notElem` fmap actionName trace

autId :: Word
autId = 8888

-- | Property that shows that an approval can be eventually generated
prop_eventuallyApproval :: Property
prop_eventuallyApproval = prop_eventually ("approve" .@ autId)

prop_eventually :: ActionName -> Property
prop_eventually anActName
  = withMaxSuccess 10000
  $ expectFailure
  $ forAllTraces (Desired 100)
                 (votingModelGenerators vks autId)
                 (initialVotingModelMem aTauV initialStakeDist 3 10 0)
                 (votingModel autId)
                 noApproval
  where
    noApproval trace = anActName `notElem` fmap actionName trace

prop_eventuallyRejection :: Property
prop_eventuallyRejection = prop_eventually ("reject" .@ autId)
