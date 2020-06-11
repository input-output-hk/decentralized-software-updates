{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Update.UnitTests.Activation where

import           Control.Arrow ((>>>))
import           Control.Monad.Reader (asks)
import           Control.Monad.State (gets)
import           Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Test.Tasty (TestTree)

import qualified Cardano.Ledger.Update as Update

import           Test.Cardano.Ledger.UpdateSpec

import           Test.Cardano.Ledger.Update.Interface hiding (tickTill)
import           Test.Cardano.Ledger.Update.TestCase
import           Test.Cardano.Ledger.Update.UnitTests.Approval
import           Test.Cardano.Ledger.Update.UnitTests.Common
import           Test.Cardano.Ledger.Update.UnitTests.Ideation

import           Cardano.Ledger.Update.Proposal (Protocol, _id)
import qualified Cardano.Ledger.Update.Proposal as Proposal

import           Test.Cardano.Ledger.Update.Data

runTests :: [TestTree]
runTests =
  with initialEnv
       [ ( "Protocol version is changed once"
         , simpleVersionChange
         )
       , ( "Protocol version is changed twice"
         , changeVersionTwice
         )
       , ( "Update is preempted by one with higher priority"
         , versionChangePreemption
         )
       , ( "Old proposal with same version gets canceled"
         , competingProposals
         )
       , ( "A proposal with lower priority (higher version) gets queued"
         , queuedProposal
         )
       , ( "A proposal without its dependencies met gets immediately queued"
         , queuedProposal'
         )
       , ( "A candidate without enough endorsements is not adopted"
         , expiredCandidate
         )
       -- Bad weather scenarios
       , ( "Endorsements of non-candidates are not allowed"
         , nonCandidateEndorsement
         )

       ]
  where
    initialEnv =
      TestCaseEnv
      { tcK                     = 2
      , tcAdversarialStakeRatio = 0.3
      , tcSIPExperts            =
          mkVotingBehavior 0  8 2 (MockVoter . mkParticipant)
      , tcImplExperts           =
          mkVotingBehavior 10 8 2 (MockVoter . mkParticipant)
      , tcStakePools            =
          mkVotingBehavior 20 8 2 (MockEndorser . mkParticipant)
      }



--------------------------------------------------------------------------------
-- Activation test cases
--------------------------------------------------------------------------------

-- | The update system should be able to allow an update proposal to be adopted
-- after going through all the update phases.
simpleVersionChange :: TestCase
simpleVersionChange = do
  update <- mkUpdate 1 (mkParticipant 0) (`increaseVersion` 1)
  approveSIP update
  tickTillStable
  approveImplementation update
  stateOf update `shouldBe` BeingEndorsed
  activate update
  stateOf update `shouldBe` Activated
  iStateProtocolVersion `shouldBe` protocolVersion update

-- | Activate the update.
--
-- Depending on the values of @k@, the number of slots per epoch, and the
-- current slot it might be possible that we are past the cut-off slot for that
-- epoch. Therefore we go to the next epoch and then endorse. The success of
-- this test case depends on having a next endorsement epoch.
activate :: UpdateSpec -> TestCase
activate update = do
  tickTillNextEpoch
  endorseTillApproval update
  tickTillNextEpoch

changeVersionTwice :: TestCase
changeVersionTwice = do
  update0 <- mkUpdate 1 (mkParticipant 0) (`increaseVersion` 1)
  approveSIP update0
  tickTillStable
  approveImplementation update0
  activate update0
  iStateProtocolVersion `shouldBe` protocolVersion update0
  update1 <- mkUpdate 2 (mkParticipant 1) (`increaseVersion` 1)
  approveSIP update1
  tickTillStable
  approveImplementation update1
  activate update1
  iStateProtocolVersion `shouldBe` protocolVersion update1

-- | Move a proposal till the endorsement phase. Then approve the implementation
-- of another proposal with higher priority (lower protocol version). Assert
-- that the latter gets activated.
versionChangePreemption :: TestCase
versionChangePreemption = do
  update0 <- mkUpdate 1 (mkParticipant 0) (`increaseVersion` 2)
  approveSIP update0
  tickTillStable
  approveImplementation update0
  stateOf update0 `shouldBe` BeingEndorsed
  -- @update1@ has higher priority than @update0@ since it increases the current
  -- version by @1@ instead of @2@.
  update1 <- mkUpdate 2 (mkParticipant 1) (`increaseVersion` 1)
  approveSIP update1
  tickTillStable
  approveImplementation update1
  -- @update0@ increases the major version, and has therefore a lower priority.
  -- Hence @update0@ enters the endorsement period.
  stateOf update1 `shouldBe` BeingEndorsed
  activate update1
  iStateProtocolVersion `shouldBe` protocolVersion update1


-- | Here we check that approving a proposal with the same version as a proposal
-- being endorsed causes the former to be canceled.
competingProposals :: TestCase
competingProposals = do
  update0 <- mkUpdate 1 (mkParticipant 0) (`increaseVersion` 1)
  update1 <- mkUpdate 2 (mkParticipant 1) (`increaseVersion` 1)
  -- @update0@ and @update1@ have the same protocol version, therefore they
  -- conflict.
  approveSIP update0
  tickTillStable
  approveSIP update1
  tickTillStable
  approveImplementation update0
  tickTillStable
  stateOf update0 `shouldBe` BeingEndorsed
  approveImplementation update1
  stateOf update1 `shouldBe` BeingEndorsed
  activate update1

queuedProposal :: TestCase
queuedProposal = do
  update0 <- mkUpdate 1 (mkParticipant 0) (`increaseVersion` 1)
  let update1 = mkUpdateThatDependsOn update0 (mkParticipant 1) (`increaseVersion` 3)
  approveSIP update0
  approveSIP update1
  tickTillStable
  approveImplementation update0
  tickTillStable
  stateOf update0 `shouldBe` BeingEndorsed
  approveImplementation update1
  -- Bear in mind that approving an implementation causes time to pass, and
  -- therefore it might cause a candidate proposal to be expired (depending on
  -- the values of parameters such as the stability-window and number of slots
  -- per-epoch)..
  tickTillStable
  stateOf update1 `shouldBe` Queued

-- | Variant of 'queuedProposal'' where an update proposal gets immediately
-- queued even if the queue is empty, since the proposal it depends on has not
-- been adopted yet.
queuedProposal' :: TestCase
queuedProposal' = do
  update0 <- mkUpdate 1 (mkParticipant 0) (`increaseVersion` 1)
  let update1 = mkUpdateThatDependsOn update0 (mkParticipant 1) (`increaseVersion` 3)
  approveSIP update0
  tickTillStable
  approveSIP update1
  tickTillStable
  approveImplementation update1
  tickTillStable
  stateOf update1 `shouldBe` Queued
  approveImplementation update0
  tickTillStable
  stateOf update0 `shouldBe` BeingEndorsed

expiredCandidate :: TestCase
expiredCandidate = do
  update <- mkUpdate 1 (mkParticipant 0) (`increaseVersion` 1)
  approveSIP update
  tickTillStable
  approveImplementation update
  tickTillStable
  stateOf update `shouldBe` BeingEndorsed
  endOfSafetyLag <- gets ( fromMaybe (error "The update should be a candidate")
                         . Update.candidateEndOfSafetyLag
                         )
  tickTill endOfSafetyLag
  stateOf update `shouldBe` ActivationExpired

nonCandidateEndorsement :: TestCase
nonCandidateEndorsement = do
  update <- mkUpdate 1 (mkParticipant 0) (`increaseVersion` 1)
  (endorseTillApproval update)
    `throwsErrorWhere` ( Update.endorsedVersionError
                        >>> (== Just (protocolVersion update))
                       )

--------------------------------------------------------------------------------
-- eDSL for describing test cases
--------------------------------------------------------------------------------

endorseTillApproval :: UpdateSpec -> TestCase
endorseTillApproval updateSpec = do
  endorsers <- asks getEndorsersForApproval
  traverse_ applyEndorsement $ fmap mkEndorsement endorsers
  where
    applyEndorsement = apply . Update.Activation
    mkEndorsement endorser
      = Update.Endorsement
        { Update.endorserId       = _id endorser
        , Update.endorsedVersion = Proposal.version (getProtocol updateSpec)
        }

getEndorsersForApproval :: TestCaseEnv -> [Endorser (Protocol MockImpl)]
getEndorsersForApproval
  = Map.keys
  . Map.filter participantApproves
  . tcStakePools
