module Cardano.Ledger.Update.UnitTests.Ideation where

import           Control.Monad.Reader (asks)
import           Data.Foldable (traverse_)
import           Test.Tasty (TestTree)

import           Cardano.Ledger.Spec.Classes.HasSigningScheme (SKey, VKey)

import           Cardano.Ledger.Spec.State.ProposalState
                     (getVotingPeriodDuration)
import           Cardano.Ledger.Spec.State.ProposalState (Decision (Approved))
import           Cardano.Ledger.Spec.STS.Update.Data (Confidence (For))
import qualified Cardano.Ledger.Spec.STS.Update.Ideation.Data as Ideation.Data

import           Cardano.Ledger.Mock (Mock)

import           Cardano.Ledger.Update.Interface
import           Cardano.Ledger.Update.TestCase
import           Cardano.Ledger.Update.UnitTests.Common
import           Cardano.Ledger.UpdateSpec

import qualified Cardano.Ledger.Update as Update


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
    , tcSIPExperts            = mkParticipantVotingBehavior 0  8 2
    , tcImplExperts           = mkParticipantVotingBehavior 10 8 2
    , tcStakePools            = mkParticipantVotingBehavior 20 8 2
    }

simpleSIPApproval :: TestCase
simpleSIPApproval = do
  update <- mkUpdate 0 IncreaseMinor
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
  tickFor $ getVotingPeriodDuration (getSIP update)
  tickTillStable -- Here the end of the voting period duration is stable, and
                 -- tally takes place.

-- | Feed an SIP payload to the STS.
sip :: UpdateAction -> UpdateSpec -> TestCase
sip Submit updateSpec
  = apply
  $ Update.Ideation
  $ Ideation.Data.Submit (getSIPCommit updateSpec) (getSIP updateSpec)
sip Reveal updateSpec
  = apply
  $ Update.Ideation
  $ Ideation.Data.Reveal
  $ getSIP updateSpec
sip Approve updateSpec = do
  approvers <- asks getSIPApprovers
  traverse_ applyVote $ fmap mkVote approvers
  where
    applyVote = apply . Update.Ideation . Ideation.Data.Vote
    mkVote (voterSKey, voterVKey)
      = Ideation.Data.mkVoteForSIP voterSKey voterVKey For (getSIPHash updateSpec)
sip Reject _updateSpec = error "SIP rejection not implemented yet"
sip Abstain _updateSpec = error "SIP abstention not implemented yet"

-- | Get the signing and verifying keys of the stakeholders whose vote will
-- guarantee the approval of an SIP proposal.
getSIPApprovers :: TestCaseEnv -> [(SKey Mock, VKey Mock)]
getSIPApprovers = getApprovers . tcSIPExperts
