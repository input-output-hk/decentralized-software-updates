{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Spec.STS.Update.Approval.UnitTests where

import           Prelude hiding ((!!))

import           Control.Monad (foldM, (<=<))
import           Data.Either (isRight)
import           Test.QuickCheck (Property, counterexample, property, (===))
import           Test.Tasty (TestTree)
import           Test.Tasty.QuickCheck (testProperty)

import           Cardano.Crypto.DSIGN.Mock (SignKeyDSIGN (SignKeyMockDSIGN),
                     VerKeyDSIGN (VerKeyMockDSIGN))

import           Control.State.Transition (PredicateFailure, STS)

import           Ledger.Core (Slot, (*.), (+.))

import           Cardano.Ledger.Spec.Classes.Hashable (Hash, hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (SKey, VKey, sign)

import           Cardano.Ledger.Spec.State.ProposalsState (decision)
import           Cardano.Ledger.Spec.State.ProposalState (Decision (Accepted, Expired, NoQuorum, Rejected, Undecided),
                     HasVotingPeriod, getVotingPeriodDuration)
import           Cardano.Ledger.Spec.STS.Update
                     (PredicateFailure (ApprovalFailure), UPDATE,
                     UpdatePayload (Approval, Ideation))
import           Cardano.Ledger.Spec.STS.Update.Approval (PredicateFailure (NoApprovedSIP, NoStableAndCommittedImpl, VotePeriodHasEnded, VotePeriodHasNotStarted),
                     ipsst)
import           Cardano.Ledger.Spec.STS.Update.Approval.Data
                     (Implementation (Implementation),
                     ImplementationData (ImplementationData),
                     Payload (Reveal, Submit), commit, implAuthor,
                     implDataSIPHash, implDataVPD, implPayload, implSalt,
                     sAuthor, sig)
import qualified Cardano.Ledger.Spec.STS.Update.Approval.Data as Approval.Data
import           Cardano.Ledger.Spec.STS.Update.Data
                     (Confidence (Abstain, Against, For))
import           Cardano.Ledger.Spec.STS.Update.Data.Commit (calcCommit)
import           Cardano.Ledger.Spec.STS.Update.Ideation.Data (IdeationPayload,
                     SIP (SIP), SIPHash (SIPHash), sipAuthor, sipHash,
                     sipPayload, sipSalt)
import qualified Cardano.Ledger.Spec.STS.Update.Ideation.Data as Ideation.Data

--  For now we're working with the state machine tests elaboration functions. If
--  needed these could be factored out.
import           Cardano.Ledger.Spec.STS.SM.Update.Ideation.Properties.Conformance
import           Cardano.Ledger.Test.Mock (Mock)
import           Cardano.Ledger.Update.Interface

-- | Entry point for the tests.
runTests :: [TestTree]
runTests =
  [ testProperty
      "SIP approval succeeds"
      $ runTest $ sipApproval (mkIState initParams) sip0
  , testProperty
      "Implementation submission succeeds"
      $ runTest $ submissionOnly (mkIState initParams) impl0
  , testProperty
      "Implementation with approved SIP gets accepted"
       $ implApproval (mkIState initParams) sip0 impl0 0
        `finalStateShould`
        contain impl0 Accepted
  , testProperty
      "Implementation with approved SIP gets rejected"
       $ implRejection (mkIState initParams) sip0 impl0
        `finalStateShould`
        contain impl0 Rejected
  , testProperty
      "Implementation with abstentions SIP gets no-quorum"
       $ implNoQuorum (mkIState initParams) sip0 impl0 0
        `finalStateShould`
        contain impl0 NoQuorum
  , testProperty
      "Implementation without enough approvals SIP gets undecided"
       $ implExpired (mkIState initParams) sip0 impl0 0
        `finalStateShould`
        contain impl0 Undecided
  , testProperty
      "Implementation without enough approvals after the maximum number of voting periods  SIP gets expired"
       $ implExpired (mkIState initParams) sip0 impl0 1
        `finalStateShould`
        contain impl0 Expired
  , testProperty
      "Implementation with approved SIP gets accepted, in second voting period"
       $ implApproval (mkIState initParams) sip0 impl0 1
        `finalStateShould`
        contain impl0 Accepted
  , testProperty
      "Implementation does not consider votes from previous voting period"
       $ insufficientVotesOverTwoPeriods (mkIState initParams) sip0 impl0
        `finalStateShould`
        contain impl0 Expired
  , testProperty
      "Implementation reveal without approved SIP fails"
      $ ensureFailure revealWithoutApproval
                      $ revealFails (mkIState initParams) impl0
  , testProperty
      "Unstable reveals are rejected"
      $ ensureFailure noStableAndCommitedImpl
                      $ revealTooEarly (mkIState initParams) sip0 impl0
  , testProperty
      "Votes before the start of the voting period are rejected"
      $ ensureFailure votePeriodHasNotStarted
                     $ voteBeforeStartOfVotingPeriod (mkIState initParams) sip0 impl0
  , testProperty
      "Votes before the start of the voting period are rejected"
      $ ensureFailure votePeriodHasEnded
                     $ voteAfterEndOfVotingPeriod (mkIState initParams) sip0 impl0

  ]

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

sipApproval
  :: IState Mock
  -> SIP Mock
  -> Either (UIError Mock) (IState Mock)
sipApproval s0 aSIP = do
  s1 <- applyIdeationPayload s0 $ Ideation.Data.Submit aSIPCommit aSIP
  -- Calculate how much time should pass till the submission is stable.
  s2 <- tick2k s1
  s3 <- applyIdeationPayload s2 $ Ideation.Data.Reveal aSIP
  s4 <- tick2k s3
  s5 <- foldM applyIdeationPayload s4 [ (voteForSIPBy 0)
                                      , (voteForSIPBy 1)
                                      , (voteForSIPBy 2)
                                      , (voteForSIPBy 3)
                                      , (voteForSIPBy 4)
                                      , (voteForSIPBy 5)
                                      , (voteForSIPBy 6)
                                      , (voteForSIPBy 7)
                                      , (voteForSIPBy 8)
                                      ]
  tickToTally aSIP s5
  where
    aSIPCommit = Ideation.Data.SIPCommit
                   aCommit
                   anAuthor
                   (sign aCommit aSKey)
      where
        aCommit  = calcCommit aSIP
        anAuthor = Ideation.Data.sipAuthor aSIP
        aSKey    = vKeyToSKey anAuthor
    voteForSIPBy i
      = Ideation.Data.Vote
      $ Ideation.Data.VoteForSIP
          { Ideation.Data.votedsipHash = sipHash aSIP
          , Ideation.Data.confidence   = For
          , Ideation.Data.voter        = participantToVKey i
          , Ideation.Data.voterSig     = sign (sipHash aSIP, For, participantToVKey i)
                                              (participantToSKey i)
          }

submissionOnly
  :: IState Mock
  -> Implementation Mock
  -> Either (UIError Mock) (IState Mock)
submissionOnly s0 anImpl = do
  let anImplCommit = calcCommit anImpl
      anImplAuthor = implAuthor anImpl
  s1 <- applyApprovalPayload
          s0
          Submit { commit  = anImplCommit
                 , sAuthor = anImplAuthor
                 , sig     = sign anImplCommit (vKeyToSKey anImplAuthor)
                 }
  pure s1

revealFails
  :: IState Mock
  -> Implementation Mock
  -> Either (UIError Mock) (IState Mock)
revealFails s0 anImpl = do
  s1 <- submissionOnly s0 anImpl
  -- Get the slot at which the submission should be stable based on the
  -- state information
  s2 <- tick2k s1
  s3 <- applyApprovalPayload s2 (Reveal anImpl)
  pure s3

implApproval
  :: IState Mock
  -> SIP Mock
  -> Implementation Mock
  -> Int
  -> Either (UIError Mock) (IState Mock)
implApproval s0 aSIP anImpl n =
  voteForImpl s0 aSIP anImpl votes n
  where
    votes = mkVotesFor (hash (implPayload anImpl)) For [0..7]

implRejection
  :: IState Mock
  -> SIP Mock
  -> Implementation Mock
  -> Either (UIError Mock) (IState Mock)
implRejection s0 aSIP anImpl =
  voteForImpl s0 aSIP anImpl votes 0
  where
    votes = mkVotesFor (hash (implPayload anImpl)) Against [0..7]

implNoQuorum
  :: IState Mock
  -> SIP Mock
  -> Implementation Mock
  -> Int
  -> Either (UIError Mock) (IState Mock)
implNoQuorum s0 aSIP anImpl n =
  voteForImpl s0 aSIP anImpl votes n
  where
    votes = mkVotesFor (hash (implPayload anImpl)) Abstain [0..7]

implExpired
  :: IState Mock
  -> SIP Mock
  -> Implementation Mock
  -> Int
  -> Either (UIError Mock) (IState Mock)
implExpired s0 aSIP anImpl n =
  voteForImpl s0 aSIP anImpl votes n
  where
    votes = mkVotesFor (hash (implPayload anImpl)) Abstain [0..3]

insufficientVotesOverTwoPeriods
  :: IState Mock
  -> SIP Mock
  -> Implementation Mock
  -> Either (UIError Mock) (IState Mock)
insufficientVotesOverTwoPeriods s0 aSIP anImpl = do
  -- We can't insufficient votes for approval.
  let votes = mkVotesFor (hash (implPayload anImpl)) For [0 .. 4]
  s1 <- voteForImpl s0 aSIP anImpl votes 0
  -- We should be in the next voting period, so we vote again.
  --
  -- We again don't cast enough votes for approval, so the end state should be
  -- undecided, provided that the votes of the previous voting period were not
  -- carried over.
  let votes' = mkVotesFor (hash (implPayload anImpl)) For [5 .. 7]
  s2 <- foldM applyApprovalPayload s1 votes'
  -- We tick so that votes are tallied
  tickToTallyN 1 anImpl s2

revealTooEarly
  :: IState Mock
  -> SIP Mock
  -> Implementation Mock
  -> Either (UIError Mock) (IState Mock)
revealTooEarly s0 aSIP anImpl = do
  s1 <- sipApproval s0 aSIP
  s2 <- submissionOnly s1 anImpl
  -- We try to reveal as soon as a commit is submitted.
  applyApprovalPayload s2 (Reveal anImpl)

voteBeforeStartOfVotingPeriod
  :: IState Mock
  -> SIP Mock
  -> Implementation Mock
  -> Either (UIError Mock) (IState Mock)
voteBeforeStartOfVotingPeriod s0 aSIP anImpl = do
  s1 <- implCommitReveal s0 aSIP anImpl
  -- We try to vote right away, without waiting for stability of the revelation.
  let votes = mkVotesFor (hash (implPayload anImpl)) Abstain [0]
  foldM applyApprovalPayload s1 votes

voteAfterEndOfVotingPeriod
  :: IState Mock
  -> SIP Mock
  -> Implementation Mock
  -> Either (UIError Mock) (IState Mock)
voteAfterEndOfVotingPeriod s0 aSIP anImpl = do
  s1 <- implCommitReveal s0 aSIP anImpl
  -- We advance the clock till the voting period starts (which means the
  -- revelation is stable).
  s2 <- tick2k s1
  -- We advance the clock till the end of the voting period.
  let vpd = getVotingPeriodDuration anImpl
  s3 <- slotTick (iStateCurrentSlot s2 +. vpd) s2
  -- We try to vote after the voting period ends.
  let votes = mkVotesFor (hash (implPayload anImpl)) Abstain [0]
  foldM applyApprovalPayload s3 votes

--------------------------------------------------------------------------------
-- Crypto omniscient functions
--------------------------------------------------------------------------------

vKeyToSKey :: VKey Mock -> SKey Mock
vKeyToSKey (VerKeyMockDSIGN p) = SignKeyMockDSIGN p

--------------------------------------------------------------------------------
-- Mock data to be used in the tests
--------------------------------------------------------------------------------

sip0 :: SIP Mock
sip0 = SIP { sipHash = sip0Hash
           , sipAuthor = participantToVKey 83
           , sipSalt = 33
           , sipPayload = sip0Data
           }
  where
    sip0Hash = SIPHash $ hash sip0Data
    sip0Data = sipPayload $ lookupSIP (sipMapping initParams) 0

impl0 :: Implementation Mock
impl0 = Implementation { implAuthor = participantToVKey 71
                       , implSalt = 29
                       , implPayload = implData
                       }
  where
    implData = ImplementationData { implDataSIPHash = sipHash sip0
                                  , implDataVPD = 10
                                  }

--------------------------------------------------------------------------------
-- Clock ticking functions
--------------------------------------------------------------------------------

-- | At which slot is will the current slot (according to the given state)
-- become stable.
stableAt :: IState Mock -> Slot
stableAt st = iStateCurrentSlot st +. 2 *. iStateK st

--------------------------------------------------------------------------------
-- Assertions
--------------------------------------------------------------------------------

ensureFailure
  :: (UIError Mock -> Bool)
  -> Either (UIError Mock) (IState Mock)
  -> Property
ensureFailure _ (Right st) =
  counterexample (show st) False
ensureFailure expectedError (Left err) =
  counterexample (show err) $ expectedError err

revealWithoutApproval :: UIError Mock -> Bool
revealWithoutApproval (UpdateError [pfs]) = any noApprovedSIP pfs
revealWithoutApproval _ = False

noStableAndCommitedImpl :: UIError Mock -> Bool
noStableAndCommitedImpl (UpdateError [pfs]) = any err pfs
  where
    err (ApprovalFailure (NoStableAndCommittedImpl _ _)) = True
    err _                                                = False
noStableAndCommitedImpl _ = False

noApprovedSIP :: PredicateFailure (UPDATE Mock) -> Bool
noApprovedSIP (ApprovalFailure (NoApprovedSIP _ _)) = True
noApprovedSIP _                                     = False

votePeriodHasNotStarted :: UIError Mock -> Bool
votePeriodHasNotStarted (UpdateError [pfs]) = any err pfs
  where
    err (ApprovalFailure (VotePeriodHasNotStarted _ _ _)) = True
    err _                                                 = False
votePeriodHasNotStarted _ = False

votePeriodHasEnded :: UIError Mock -> Bool
votePeriodHasEnded (UpdateError [pfs]) = any err pfs
  where
    err (ApprovalFailure (VotePeriodHasEnded _ _ _)) = True
    err _                                                 = False
votePeriodHasEnded _ = False

--------------------------------------------------------------------------------
-- Auxiliary functions
--------------------------------------------------------------------------------

implCommitReveal
  :: IState Mock
  -> SIP Mock
  -> Implementation Mock
  -> Either (UIError Mock) (IState Mock)
implCommitReveal s0 aSIP anImpl = do
  s1 <- sipApproval s0 aSIP
  s2 <- submissionOnly s1 anImpl
  s3 <- tick2k s2
  applyApprovalPayload s3 (Reveal anImpl)

-- | Create votes for the given implementation hash, with the given confidence,
-- using participants from 0 to the given upper bound.
--
mkVotesFor
  :: Hash Mock (ImplementationData Mock)
  -> Confidence
  -> [Word]
  -- ^ Participants id's
  -> [Payload Mock]
mkVotesFor anImplHash confidence participantsIds =
  fmap (uncurry $ voteForImplBy anImplHash)
                 (zip (repeat confidence) participantsIds)

voteForImpl
  :: IState Mock
  -> SIP Mock
  -> Implementation Mock
  -> [Payload Mock]
  -> Int
  -- ^ How many voting periods to wait
  -> Either (UIError Mock) (IState Mock)
voteForImpl s0 aSIP anImpl votes n = do
  s4 <- implCommitReveal s0 aSIP anImpl
  -- Advance the clock till the revelation slot is stable, which means that the
  -- voting period has started.
  s5 <- tickToTallyN n anImpl s4
  s6 <- slotTick (stableAt s5) s5
  s7 <- foldM applyApprovalPayload s6 votes
  tickToTally anImpl s7

tickToTallyN
  :: HasVotingPeriod d
  => Int
  -- | How many times should we tick to tally.
  -> d
  -> IState Mock
  -> Either (UIError Mock) (IState Mock)
tickToTallyN n d s0 =
  threadM (replicate n (tickToTally d)) s0
  where
    threadM = foldr (<=<) pure

-- | Tick slots till the slot at which the end of the voting period is stable.
tickToTally
  :: HasVotingPeriod d
  => d
  -> IState Mock
  -> Either (UIError Mock) (IState Mock)
tickToTally d s0  = do
  let vpd = getVotingPeriodDuration d
  s1 <- slotTick (iStateCurrentSlot s0 +. vpd) s0
  -- Tick the stability of the end of the voting period, which should trigger
  -- the tally.
  slotTick (stableAt s1) s1

tick2k :: IState Mock -> Either (UIError Mock) (IState Mock)
tick2k st = slotTick (stableAt st) st

voteForImplBy
  :: Hash Mock (ImplementationData Mock)
  -- ^ Implementation being voted for.
  -> Confidence
  -> Word
  -- ^ Participant that casts the vote.
  -> Payload Mock
voteForImplBy anImplHash confidence i
  = Approval.Data.Vote
  $ Approval.Data.ImplVote
      { Approval.Data.vImplHash  = anImplHash
      , Approval.Data.confidence = confidence
      , Approval.Data.vAuthor    = participantToVKey i
      , Approval.Data.vSig       = sign ( anImplHash
                                        , confidence
                                        , participantToVKey i
                                        )
                                        (participantToSKey i)
      }

runTest :: Either (UIError Mock) (IState Mock) -> Property
runTest e =
  counterexample (show e) $ isRight e

finalStateShould
  :: Either (UIError Mock) (IState Mock)
  -> (IState Mock -> Property)
  -> Property
finalStateShould (Left err) _ = counterexample (show err) $ property False
finalStateShould (Right st) p = counterexample (show st)  $ p st

contain
  :: Implementation Mock
  -> Decision
  -> IState Mock
  -> Property
contain anImpl status st
  = decision (hash $ implPayload anImpl) (ipsst $ iStateApproval st) === status

applyApprovalPayload
  :: forall p
   . STS (UPDATE p)
  => IState p
  -> Approval.Data.Payload p
  -> Either (UIError p) (IState p)
applyApprovalPayload st p = applyUpdatePayload (Approval p) st

applyIdeationPayload
  :: forall p
   . STS (UPDATE p)
  => IState p
  -> IdeationPayload p
  -> Either (UIError p) (IState p)
applyIdeationPayload st p = applyUpdatePayload (Ideation p) st
