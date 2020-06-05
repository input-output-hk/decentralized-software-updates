module Cardano.Ledger.Update.UnitTests.Approval where

import           Control.Arrow ((>>>))
import           Control.Monad.Reader (asks)
import           Control.Monad.State (gets, modify')
import           Data.Foldable (traverse_)
import           Test.Tasty (TestTree)

import           Ledger.Core (SlotCount (SlotCount))

import           Cardano.Ledger.Mock (Mock)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (SKey, VKey)
import           Cardano.Ledger.Spec.Classes.TracksSlotTime (stableAfter)
import           Cardano.Ledger.Spec.State.ProposalState (Decision (Approved, Expired, Rejected, Undecided, WithNoQuorum))
import           Cardano.Ledger.Spec.State.ProposalState
                     (getVotingPeriodDuration)
import qualified Cardano.Ledger.Spec.State.ProposalState as ProposalState

import qualified Cardano.Ledger.Spec.STS.Update.Approval.Data as Approval.Data
import           Cardano.Ledger.Spec.STS.Update.Data (Confidence)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Confidence

import           Cardano.Ledger.Update.Interface
import           Cardano.Ledger.Update.TestCase
import           Cardano.Ledger.Update.UnitTests.Common
import           Cardano.Ledger.UpdateSpec

import           Cardano.Ledger.Update.UnitTests.Ideation

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
    , tcSIPExperts            = mkParticipantVotingBehavior 0  8 2
    , tcImplExperts           = mkParticipantVotingBehavior 10 8 2
    , tcStakePools            = mkParticipantVotingBehavior 20 8 2
    }


simpleImplApproval :: TestCase
simpleImplApproval = do
  update <- mkUpdate 0 IncreaseMinor
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
  update <- mkUpdate 0 IncreaseMinor
  approveSIP update
  tickTillStable
  stateOf update `shouldBe` SIP (IsStably Approved)
  submit `implementation` update
  tickTillStable
  reveal  `implementation` update
  tickTillStable
  reject `implementation` update
  tickFor $ getVotingPeriodDuration (getImpl update)
  tickTillStable
  stateOf update `shouldBe` Implementation (Is Rejected)

simpleNoQuorum :: TestCase
simpleNoQuorum = do
  update <- mkUpdate 0 IncreaseMinor
  approveSIP update
  tickTillStable
  stateOf update `shouldBe` SIP (IsStably Approved)
  submit `implementation` update
  tickTillStable
  reveal  `implementation` update
  tickTillStable
  abstain `implementation` update
  tickFor $ getVotingPeriodDuration (getImpl update)
  tickTillStable
  stateOf update `shouldBe` Implementation (Is WithNoQuorum)

simpleExpiration :: TestCase
simpleExpiration = do
  update <- mkUpdate 0 IncreaseMinor
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
  let votingPeriodDuration = getVotingPeriodDuration (getImpl update)
      votingPeriods' = SlotCount $ fromIntegral $ ProposalState.unVotingPeriod votingPeriods
  tickFor $ votingPeriods' * (votingPeriodDuration + stableAfterSt)
  stateOf update `shouldBe` Implementation (Is Expired)

implApprovalInSecondVotingRound :: TestCase
implApprovalInSecondVotingRound = do
  update <- mkUpdate 0 IncreaseMinor
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
  vote approvers0 update Confidence.For
  tickFor $ getVotingPeriodDuration (getImpl update)
  tickTillStable
  -- The tally point is reached. The implementation should not have enough
  -- votes.
  stateOf update `shouldBe` (Implementation (Is Undecided))
  approve `implementation` update
  stateOf update `shouldBe` Implementation (Is Undecided)
  tickFor $ getVotingPeriodDuration (getImpl update)
  tickTillStable
  stateOf update `shouldBe` BeingEndorsed

implVotesAreNotCarriedOver :: TestCase
implVotesAreNotCarriedOver = do
  -- We make sure that we have two voting periods, so that the proposal is
  -- expired at the end of the second voting period.
  modify' (\st -> st { iStateMaxVotingPeriods = 2})
  update <- mkUpdate 0 IncreaseMinor
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
    mid             = length approvers `div` 2
  vote approvers0 update Confidence.For
  tickFor $ getVotingPeriodDuration (getImpl update)
  tickTillStable
  stateOf update `shouldBe` (Implementation (Is Undecided))
  vote approvers1 update Confidence.For
  tickFor $ getVotingPeriodDuration (getImpl update)
  tickTillStable
  stateOf update `shouldBe` (Implementation (Is Expired))

implRevealWithoutApprovedSIP :: TestCase
implRevealWithoutApprovedSIP = do
  update <- mkUpdate 0 IncreaseMinor
  submit `implementation` update
  tickTillStable
  (reveal  `implementation` update)
    `throwsErrorWhere` ( Update.noApprovedSIP
                         >>> (== Just (getSIPHash update))
                       )

revealOfAnUnstableImplSubmission :: TestCase
revealOfAnUnstableImplSubmission = do
  update <- mkUpdate 0 IncreaseMinor
  approveSIP update
  tickTillStable
  submit `implementation` update
  -- We reveal immediately, without waiting for stability.
  (reveal  `implementation` update)
    `throwsErrorWhere` ( Update.noStableImplementationCommit
                        >>> (== Just (getImpl update))
                       )

voteBeforeTheStartOfVotingPeriod :: TestCase
voteBeforeTheStartOfVotingPeriod = do
  update <- mkUpdate 0 IncreaseMinor
  approveSIP update
  tickTillStable
  submit `implementation` update
  tickTillStable
  reveal  `implementation` update
  -- Here we don't want for the stabilization of the reveal message to start
  -- voting.
  (approve `implementation` update)
    `throwsErrorWhere` ( Update.implementationVotePeriodHasNotStarted
                         >>> (== Just (getImplHash update))
                       )

voteAfterTheEndOfVotingPeriod :: TestCase
voteAfterTheEndOfVotingPeriod = do
  update <- mkUpdate 0 IncreaseMinor
  approveSIP update
  tickTillStable
  submit `implementation` update
  tickTillStable
  reveal  `implementation` update
  tickTillStable                                     -- Here the voting period starts
  tickFor $ getVotingPeriodDuration (getImpl update) -- Here the voting period ends
  (approve `implementation` update)
    `throwsErrorWhere` ( Update.implementationVotePeriodHasEnded
                        >>> (== Just (getImplHash update))
                       )


-- Precondition: the update SIP should be stably approved
approveImplementation :: UpdateSpec -> TestCase
approveImplementation update = do
  stateOf update `shouldBe` SIP (IsStably Approved)
  submit `implementation` update
  tickTillStable
  reveal  `implementation` update
  stateOf update `shouldBe` Implementation (Is Undecided)
  tickTillStable
  approve `implementation` update
  stateOf update `shouldBe` Implementation (Is Undecided)
  tickFor $ getVotingPeriodDuration (getImpl update)
  tickTillStable


-- | Feed an implementation payload to the STS.
implementation :: UpdateAction -> UpdateSpec -> TestCase
implementation Submit updateSpec
  = apply
  $ Update.Approval
  $ Approval.Data.Submit
  $ Approval.Data.mkSubmission (getImplAuthorSKey updateSpec)
                               (getImpl updateSpec)
implementation Reveal updateSpec
  = apply
  $ Update.Approval
  $ Approval.Data.Reveal (getImpl updateSpec)
implementation Approve updateSpec = updateSpec `approversVote` Confidence.For
implementation Reject updateSpec = updateSpec `approversVote` Confidence.Against
implementation Abstain updateSpec = updateSpec `approversVote` Confidence.Abstain

approversVote :: UpdateSpec -> Confidence -> TestCase
approversVote updateSpec confidence
  = do
  approvers <- asks getImplApprovers
  traverse_ applyVote $ fmap mkVote approvers
  where
    applyVote = apply . Update.Approval . Approval.Data.Vote
    mkVote (voterSKey, voterVKey)
      = Approval.Data.mkImplVote voterSKey voterVKey confidence (getImplHash updateSpec)

vote :: [(SKey Mock, VKey Mock)] -> UpdateSpec -> Confidence -> TestCase
vote voters updateSpec confidence =
  traverse_ applyVote $ fmap mkVote voters
  where
    applyVote = apply . Update.Approval . Approval.Data.Vote
    mkVote (voterSKey, voterVKey)
      = Approval.Data.mkImplVote voterSKey voterVKey confidence (getImplHash updateSpec)


getImplApprovers :: TestCaseEnv -> [(SKey Mock, VKey Mock)]
getImplApprovers = getApprovers . tcSIPExperts -- TODO: this should be
                                               -- 'tcImplExperts' once we
                                               -- separate the stake
                                               -- distribution into the SIP
                                               -- experts stake and the
                                               -- implementation experts stake.
