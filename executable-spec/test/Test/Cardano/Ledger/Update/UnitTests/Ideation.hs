module Test.Cardano.Ledger.Update.UnitTests.Ideation where

import           Control.Monad.Reader (asks)
import           Data.Foldable (traverse_)
import           Test.Tasty (TestTree)

import           Cardano.Ledger.Update.ProposalState (Decision (Approved))

import           Test.Cardano.Ledger.Update.Interface
import           Test.Cardano.Ledger.Update.TestCase
import           Test.Cardano.Ledger.Update.UnitTests.Common
import           Test.Cardano.Ledger.UpdateSpec

import           Cardano.Ledger.Update.Proposal (Confidence (For), Voter, _id)
import qualified Cardano.Ledger.Update.Proposal as Proposal

import qualified Cardano.Ledger.Update as Update

import           Test.Cardano.Ledger.Update.Data


runTests :: [TestTree]
runTests =
  with initialEnv
  [ ( "SIP approval succeeds"
    , simpleSIPApproval
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

simpleSIPApproval :: TestCase
simpleSIPApproval = do
  update <- mkUpdate 1 (mkParticipant 0) (`increaseVersion` 1)
  approveSIP update
  tickTillStable
  stateOf update `shouldBe` SIP (IsStably Approved)

approveSIP :: UpdateSpec -> TestCase
approveSIP update =  do
  submit `sip` update
  tickTillStable
  reveal `sip` update
  tickTillStable
  approve `sip` update
  tickFor $ Proposal.votingPeriodDuration (getSIP update)
  tickTillStable -- Here the end of the voting period duration is stable, and
                 -- tally takes place.

-- | Feed an SIP payload to the STS.
sip :: UpdateAction -> UpdateSpec -> TestCase
sip Submit updateSpec
  = apply
  $ Update.Ideation
  $ Proposal.Submit (getSIPSubmission updateSpec)
sip Reveal updateSpec
  = apply
  $ Update.Ideation
  $ Proposal.Reveal
  $ getSIPRevelation updateSpec
sip Approve updateSpec = do
  approvers <- asks getSIPApprovers
  traverse_ applyVote $ fmap mkVote approvers
  where
    applyVote = apply . Update.Ideation . Proposal.Cast
    mkVote voter
      = MockVote (_id voter) (_id (getSIP updateSpec)) For True
sip Reject _updateSpec = error "SIP rejection not implemented yet"
sip Abstain _updateSpec = error "SIP abstention not implemented yet"

-- | Get the signing and verifying keys of the stakeholders whose vote will
-- guarantee the approval of an SIP proposal.
getSIPApprovers :: TestCaseEnv -> [Voter MockSIP]
getSIPApprovers = getApprovers . tcSIPExperts
