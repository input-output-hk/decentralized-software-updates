module Test.Cardano.Ledger.Update.UnitTests.Approval where

import           Control.Arrow ((>>>))
import           Control.Monad.Reader (asks)
import           Control.Monad.State (gets, modify')
import           Data.Foldable (traverse_)
import           Test.Tasty (TestTree)

import           Cardano.Ledger.Update.Env.TracksSlotTime (stableAfter)
import           Cardano.Ledger.Update.ProposalState
                     (Decision (Approved, Expired, Rejected, WithNoQuorum))
import qualified Cardano.Ledger.Update.ProposalState as ProposalState

import           Test.Cardano.Ledger.Update.Interface
import           Test.Cardano.Ledger.Update.TestCase
import           Test.Cardano.Ledger.Update.UnitTests.Common
import           Test.Cardano.Ledger.UpdateSpec

import           Test.Cardano.Ledger.Update.UnitTests.Ideation

import           Cardano.Ledger.Update.Proposal (Confidence, Voter, _id)
import qualified Cardano.Ledger.Update.Proposal as Proposal

import           Test.Cardano.Ledger.Update.Data

import qualified Cardano.Ledger.Update as Update


runTests :: [TestTree]
runTests =
  with initialEnv
  [ ("Implementation gets expired"
    , simpleExpiration
    )
  , ("Implementation gets no quorum"
    , simpleNoQuorum
    )
  , ("Implementation gets rejected"
    , simpleRejection
    )
  , ( "Implementation gets accepted"
    , simpleImplApproval
    )
  , ( "Implementation gets accepted in the second voting round"
    , implApprovalInSecondVotingRound
    )
  , ( "Votes from previous voting periods are not carried over"
    , implVotesAreNotCarriedOver
    )
  -- Bad weather test cases.
  , ( "Implementation reveal without approved SIP fails"
    , implRevealWithoutApprovedSIP
    )
  , ( "Revelation of an unstable submission fails"
    , revealOfAnUnstableImplSubmission
    )
  , ("Votes before the start of the voting period are rejected"
    , voteBeforeTheStartOfVotingPeriod
    )
  , ("Votes before the end of the voting period are rejected"
    , voteAfterTheEndOfVotingPeriod
    )
  ]
  where
  initialEnv =
    TestCaseEnv
    { tcK                     = 3
    , tcAdversarialStakeRatio = 0.3
    , tcSIPExperts            =
        mkVotingBehavior 0  8 2 (MockVoter . mkParticipant)
    , tcImplExperts           =
        mkVotingBehavior 10 8 2 (MockVoter . mkParticipant)
    , tcStakePools            =
        mkVotingBehavior 20 8 2 (MockEndorser . mkParticipant)
    }


simpleImplApproval :: TestCase
simpleImplApproval = do
  update <- mkUpdate (SpecId 1) (mkParticipant 0) (`increaseVersion` 1)
  approveSIP update
  tickTillStable
  approveImplementation update
  stateOf update `shouldBe` BeingEndorsed -- After approving the proposal it is
                                          -- transferred to the activation
                                          -- phase, where there are no proposals
                                          -- in the queue, so the proposal is
                                          -- immediately put in the endorsement
                                          -- (sub) phase.

simpleRejection :: TestCase
simpleRejection = do
  update <- mkUpdate (SpecId 1) (mkParticipant 0) (`increaseVersion` 1)
  approveSIP update
  tickTillStable
  stateOf update `shouldBe` SIP (IsStably Approved)
  submit `implementation` update
  tickTillStable
  reveal  `implementation` update
  tickTillStable
  reject `implementation` update
  tickFor $ Proposal.votingPeriodDuration (getImpl update)
  tickTillStable
  stateOf update `shouldBe` Implementation (Is Rejected)

simpleNoQuorum :: TestCase
simpleNoQuorum = do
  update <- mkUpdate (SpecId 88) (mkParticipant 0) (`increaseVersion` 1)
  approveSIP update
  tickTillStable
  stateOf update `shouldBe` SIP (IsStably Approved)
  submit `implementation` update
  tickTillStable
  reveal  `implementation` update
  tickTillStable
  abstain `implementation` update
  tickFor $ Proposal.votingPeriodDuration (getImpl update)
  tickTillStable
  stateOf update `shouldBe` Implementation (Is WithNoQuorum)

simpleExpiration :: TestCase
simpleExpiration = do
  update <- mkUpdate (SpecId 1) (mkParticipant 0) (`increaseVersion` 1)
  approveSIP update
  tickTillStable
  stateOf update `shouldBe` SIP (IsStably Approved)
  submit `implementation` update
  tickTillStable
  reveal  `implementation` update
  tickTillExpired update
  stateOf update `shouldBe` Implementation (Is Expired)

tickTillExpired :: UpdateSpec -> TestCase
tickTillExpired update = do
  tickTillStable -- We position the slot at the beginning of the voting period.
  votingPeriods <- gets iStateMaxVotingPeriods
  stableAfterSt <- gets stableAfter
  let votingPeriodDuration = Proposal.votingPeriodDuration (getImpl update)
      votingPeriods' =  fromIntegral
                      $ ProposalState.unVotingPeriod votingPeriods
  tickFor $ votingPeriods' * (votingPeriodDuration + stableAfterSt)
  stateOf update `shouldBe` Implementation (Is Expired)

implApprovalInSecondVotingRound :: TestCase
implApprovalInSecondVotingRound = do
  update <- mkUpdate (SpecId 1) (mkParticipant 0) (`increaseVersion` 1)
  approveSIP update
  tickTillStable
  submit `implementation` update
  tickTillStable
  reveal  `implementation` update
  tickTillStable
  -- Here the voting period ends: we vote with half of the approvers
  approvers <- asks getImplApprovers
  let
    (approvers0, _) = splitAt mid approvers
    mid             = length approvers `div` 2
  vote approvers0 update Proposal.For
  tickFor $ Proposal.votingPeriodDuration (getImpl update)
  tickTillStable
  -- The tally point is reached. The implementation should not have enough
  -- votes.
  stateOf update `shouldBe` Implementation StablyRevealed
  approve `implementation` update
  stateOf update `shouldBe` Implementation StablyRevealed
  tickFor $ Proposal.votingPeriodDuration (getImpl update)
  tickTillStable
  stateOf update `shouldBe` BeingEndorsed

implVotesAreNotCarriedOver :: TestCase
implVotesAreNotCarriedOver = do
  -- We make sure that we have two voting periods, so that the proposal is
  -- expired at the end of the second voting period.
  modify' (\st -> st { iStateMaxVotingPeriods = 2})
  update <- mkUpdate (SpecId 1) (mkParticipant 0) (`increaseVersion` 1)
  approveSIP update
  tickTillStable
  submit `implementation` update
  tickTillStable
  reveal  `implementation` update
  tickTillStable
  -- Here the voting period ends: we vote with half of the approvers
  approvers <- asks getImplApprovers
  let
    (approvers0, approvers1) = splitAt mid approvers
    mid                      = length approvers `div` 2
  vote approvers0 update Proposal.For
  tickFor $ Proposal.votingPeriodDuration (getImpl update)
  tickTillStable
  stateOf update `shouldBe` Implementation StablyRevealed
  vote approvers1 update Proposal.For
  tickFor $ Proposal.votingPeriodDuration (getImpl update)
  tickTillStable
  stateOf update `shouldBe` (Implementation (Is Expired))

implRevealWithoutApprovedSIP :: TestCase
implRevealWithoutApprovedSIP = do
  update <- mkUpdate (SpecId 1) (mkParticipant 0) (`increaseVersion` 1)
  submit `implementation` update
  tickTillStable
  (reveal  `implementation` update)
    `throwsErrorWhere` ( Update.noApprovedSIP
                         >>> (== Just (getSIPId update))
                       )

revealOfAnUnstableImplSubmission :: TestCase
revealOfAnUnstableImplSubmission = do
  update <- mkUpdate (SpecId 1) (mkParticipant 0) (`increaseVersion` 1)
  approveSIP update
  tickTillStable
  submit `implementation` update
  -- We reveal immediately, without waiting for stability.
  (reveal  `implementation` update)
    `throwsErrorWhere` ( Update.noStableImplementationCommit
                        >>> (== Just (getImplRevelation update))
                       )

voteBeforeTheStartOfVotingPeriod :: TestCase
voteBeforeTheStartOfVotingPeriod = do
  update <- mkUpdate (SpecId 1) (mkParticipant 0) (`increaseVersion` 1)
  approveSIP update
  tickTillStable
  submit `implementation` update
  tickTillStable
  reveal  `implementation` update
  -- Here we don't want for the stabilization of the reveal message to start
  -- voting.
  (approve `implementation` update)
    `throwsErrorWhere` ( Update.implementationVotePeriodHasNotStarted
                         >>> (== Just (getImplId update))
                       )

voteAfterTheEndOfVotingPeriod :: TestCase
voteAfterTheEndOfVotingPeriod = do
  update <- mkUpdate (SpecId 1) (mkParticipant 0) (`increaseVersion` 1)
  approveSIP update
  tickTillStable
  submit `implementation` update
  tickTillStable
  reveal  `implementation` update
  tickTillStable
  -- Now the voting period starts.
  tickFor $ Proposal.votingPeriodDuration (getImpl update)
  -- Now the voting period ends.
  (approve `implementation` update)
    `throwsErrorWhere` ( Update.implementationVotePeriodHasEnded
                        >>> (== Just (getImplId update))
                       )


-- Precondition: the update SIP should be stably approved
approveImplementation :: UpdateSpec -> TestCase
approveImplementation update = do
  stateOf update `shouldBe` SIP (IsStably Approved)
  submit `implementation` update
  tickTillStable
  reveal  `implementation` update
  stateOf update `shouldBe` Implementation Revealed
  tickTillStable
  approve `implementation` update
  stateOf update `shouldBe` Implementation StablyRevealed
  tickFor $ Proposal.votingPeriodDuration (getImpl update)
  tickTillStable


-- | Feed an implementation payload to the STS.
implementation :: UpdateAction -> UpdateSpec -> TestCase
implementation Submit updateSpec
  = apply
  $ Update.Approval
  $ Proposal.Submit (getImplSubmission updateSpec)
implementation Reveal updateSpec
  = apply
  $ Update.Approval
  $ Proposal.Reveal (getImplRevelation updateSpec)
implementation Approve updateSpec = updateSpec `approversVote` Proposal.For
implementation Reject updateSpec = updateSpec `approversVote` Proposal.Against
implementation Abstain updateSpec = updateSpec `approversVote` Proposal.Abstain

approversVote :: UpdateSpec -> Confidence -> TestCase
approversVote updateSpec confidence = do
  approvers <- asks getImplApprovers
  vote approvers updateSpec confidence

vote :: [Voter MockImpl] -> UpdateSpec -> Confidence -> TestCase
vote voters updateSpec confidence =
  traverse_ applyVote $ fmap mkVote voters
  where
    applyVote = apply . Update.Approval . Proposal.Cast
    mkVote voter =
      MockVote (_id voter) (_id (getImpl updateSpec)) confidence True

getImplApprovers :: TestCaseEnv -> [Voter MockImpl]
getImplApprovers = getApprovers . tcImplExperts
