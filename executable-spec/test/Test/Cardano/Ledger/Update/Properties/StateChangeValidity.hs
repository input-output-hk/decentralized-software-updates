{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Update.Properties.StateChangeValidity
  (prop_updateEventTransitionsAreValid)
where

import           Control.Monad (when)
import           Data.Foldable (traverse_)
import           Data.List (nub, nubBy)
import           Data.Maybe (catMaybes)

import qualified Data.Set as Set

import           Cardano.Ledger.Assert

import           Cardano.Slotting.Slot (SlotNo)

import           Cardano.Ledger.Update.Proposal

import           Cardano.Ledger.Update hiding (Canceled, Expired)
import           Cardano.Ledger.Update.Env.HasAdversarialStakeRatio
                     (adversarialStakeRatio)
import           Cardano.Ledger.Update.Env.HasStakeDistribution
                     (HasStakeDistribution, stakeDistribution, totalStake)
import           Cardano.Ledger.Update.Env.HasVotingPeriodsCap
                     (HasVotingPeriodsCap, VotingPeriod, maxVotingPeriods)
import           Cardano.Ledger.Update.Env.StakeDistribution (Stake,
                     stakeOfKeys', stakeThreshold)
import           Cardano.Ledger.Update.Env.TracksSlotTime
import           Cardano.Ledger.Update.ProposalsState (Decision (Approved, Expired, Rejected, Undecided, WithNoQuorum))

import qualified Cardano.Ledger.Update as Update

import           Trace
import           Trace.PropertyTesting

import           Test.Cardano.Ledger.Update.Data
import           Test.Cardano.Ledger.Update.Events
import           Test.Cardano.Ledger.Update.Interface
import           Test.Cardano.Ledger.Update.Properties.SimpleScenario
import           Test.Cardano.Ledger.Update.Properties.UpdateSUT
import           Test.Cardano.Ledger.UpdateSpec


prop_updateEventTransitionsAreValid
  :: Trace UpdateSUT Simple -> Check
prop_updateEventTransitionsAreValid trace = Check $
  forall (tsUpdateSpecs (scenario trace))
         (`updateEventTransitionsForUpdateSpecAreValid` trace)

updateEventTransitionsForUpdateSpecAreValid
  :: UpdateSpec -> Trace UpdateSUT Simple -> Assertion
updateEventTransitionsForUpdateSpecAreValid updateSpec trace = do
  -- TODO: we also need coverage tests for the transitions.

  -- TODO: pending checks (which we might be forced to do outside this
  -- property):
  --
  -- - the first valid implementation submission is never rejected, and
  --   subsequent submissions of the same proposal should be.
  noUnrightfulRejectionsInUnknowState updateEventsOfUpdateSpec
  sequence_ $ mapAdjacent validateTransition updateEventsOfUpdateSpec
  where
    updateEventsOfUpdateSpec = updateEvents updateSpec trace
    -- If there are no transitions from the unknown event, then the update
    -- system should not unrightfully reject an SIP submission.
    --
    -- So if a SIP submission is rejected it can only be because its signature
    -- does not verify.
    --
    noUnrightfulRejectionsInUnknowState ((E Unknown fragment):_) = do
      -- The only action that we allow is implementation submissions. The update
      -- system has no way to determine if the submission corresponds to an
      -- approved SIP. This information is only available once we have a
      -- revelation.
      onlyImplementationSubmissionAllowed updateSpec fragment
      -- A valid SIP's submission corresponding to @updateSpec@ should not be
      -- rejected in fragment, since in @fragment@ the system under test reports
      -- that such submission didn't occur.
      forall (invalidActions fragment)
             (\act -> getSubmittedSIP act /=! Just (getSIPSubmission updateSpec)
                      ||! (not $ signatureVerifies $ getSIPSubmission updateSpec)
                          `orElseShow` "Unexpected SIP submission rejection."
             )
    noUnrightfulRejectionsInUnknowState _                        =
      -- In this case the system started with an event different from 'Unknown'.
      -- This can happen for instance if the first action of the trace is an SIP
      -- submission.
      pass

    validateTransition (E Unknown _fragment)
                       (E (SIP Submitted) fragment') = do
      -- The action that led to the @SIP Submitted@ state should be a SIP
      -- submitted.
      getSubmittedSIP (firstAction fragment')
        ==! Just (getSIPSubmission updateSpec)
      -- The first valid action of @fragment'@ must have been an SIP submission.
      -- After this action no other actions associated to @updateSpec@ are
      -- allowed, besides implementation commits.
      onlyImplementationSubmissionAllowed updateSpec
                                          (dropFirstValidAction fragment')
      fragmentLengthInSlots fragment' <! stableAfter env
      where
        env = unUpdateSt (firstState fragment')
    validateTransition (E (SIP Submitted) fragment)
                       (E (SIP StablySubmitted) fragment') = do
      -- The difference between the two states must be @stableAfter@. Note that
      -- the first state of @fragment@ contains the slot at which the SIP was
      -- submitted, and the first state of @fragment'@ contains the slot at
      -- which the SIP submission was marked as stable by the update system.
      validateStabilityEvent fragment fragment'
      -- In the fragment where the implementation is stably submitted
      -- (@fragment'@) no other events besides implementation submissions
      -- (commits) should have taken place. If we would see a SIP revelation,
      -- that would point out to an error in the state that the update system
      -- reports: throughout @fragment'@ the system did not report that the SIP
      -- revelation corresponding to @updateSpec@ took place.
      onlyImplementationSubmissionAllowed updateSpec fragment'
      -- A SIP revelation should not be unrightfully rejected while in the
      -- @StablySubmitted@ state, since the system is waiting for a revelation.
      --
      -- A revelation can be rejected only if:
      --
      -- 0. There is no corresponding commit
      --
      -- 1. There is a commit but it is not stable yet.
      --
      -- 2. The SIP was already revealed.
      --
      -- Since the SIP is stably submitted we know that that 0 and 1 are not
      -- true. So an SIP was rejected we know that either the rejection is
      -- unwarranted or the system does not correctly report when an SIP is
      -- revealed. Either way, the rejection of the SIP revelation corresponding
      -- to the update specification points to an error in the update system
      -- implementation.
      forall (invalidActions fragment')
             (\act -> getRevealedSIP act /=! Just (getSIPRevelation updateSpec)
             )
    validateTransition (E (SIP StablySubmitted) _fragment)
                       (E (SIP Revealed) fragment') = do
      -- The first action of @fragment'@ must be the SIP revelation
      -- corresponding to @updateSpec@.
      getRevealedSIP (firstAction fragment')
        ==! Just (getSIPRevelation updateSpec)
      onlyImplementationSubmissionAllowed updateSpec
                                          (dropFirstValidAction fragment')
      fragmentLengthInSlots fragment' <! stableAfter env
      where
        env = unUpdateSt (firstState fragment')
    validateTransition (E (SIP Revealed) fragment)
                       (E (SIP StablyRevealed) fragment') = do
      validateStabilityEvent fragment fragment'
      -- Once the SIP revelation is stable, only implementation commits or SIP
      -- votes are allowed. A stable revelation opens the voting period.
      onlyImplementationSubmissionOrSIPVotesAllowed
        updateSpec
        (dropFirstValidAction fragment')
      -- TODO: we need to test also that if the trace terminates in a
      -- @StablyRevealed@ state, then no verdict was possible in that fragment.
    validateTransition (E (SIP StablyRevealed) fragment)
                       (E (SIP (Is what)) fragment') = do
      validateVerdictEvent (getSIP updateSpec)
                           (getSIPVoteOf (getSIPId updateSpec))
                           fragment
                           (firstEvent fragment')
                           what
      -- We shouldn't see any actions in @fragment'@, save for implementation
      -- submissions, which the system cannot check.
      onlyImplementationSubmissionAllowed updateSpec fragment'
      when (what == Approved) $
        forall (invalidActions fragment')
               (\act -> getSubmittedImpl act
                        /=! Just (getImplSubmission updateSpec)
               )
    validateTransition (E (SIP StablyRevealed) fragment)
                       (E (Implementation Submitted) fragment') = do
      -- Function @stateOf updateSpec@ reports an implementation as @Submitted@
      -- only when the @updateSpec@ SIP was approved. Hence we have to validate
      -- such verdict.
      validateVerdictEvent (getSIP updateSpec)
                           (getSIPVoteOf (getSIPId updateSpec))
                           fragment
                           (firstEvent fragment')
                           Approved
      -- The implementation was submitted already, so we expect no actions in
      -- @fragment'@. We have to resort to the @onlyEndorsementsAreAllowed@
      -- function because we cannot check the proposal id to which an
      -- endorsement is related. So in @fragment'@ it is possible to see
      -- endorsements of proposals with the same version as @updateSpec@.
      onlyEndorsementsAreAllowed updateSpec fragment'
    validateTransition (E (SIP StablyRevealed) fragment)
                       (E (Implementation StablySubmitted) fragment') = do
      -- Here the checks are similar to the previous case.
      validateVerdictEvent (getSIP updateSpec)
                           (getSIPVoteOf (getSIPId updateSpec))
                           fragment
                           (firstEvent fragment')
                           Approved
      onlyEndorsementsAreAllowed updateSpec fragment'
    validateTransition (E (SIP (Is what)) fragment)
                       (E (SIP (IsStably what')) fragment') = do
      validateStabilityEvent fragment fragment'
      onlyImplementationSubmissionAllowed updateSpec fragment'
      -- The verdicts must be the same.
      what ==! what'
      -- It the SIP was approved, the system cannot reject a valid submission.
      when (what' == Approved) $
        forall (invalidActions fragment')
               (\act -> getSubmittedImpl act
                        /=! Just (getImplSubmission updateSpec)
               ) -- TODO: shouldn't we add here the condition that if the
                 -- implementation submission is invalid, the it can only be
                 -- because the submission does not verify. If so, why did not
                 -- get an error here? Do we generate impl submissions with
                 -- invalid signatures.
    validateTransition (E (SIP (Is Approved)) _fragment)
                       (E (Implementation Submitted) fragment') = do
      getSubmittedImpl (firstAction fragment')
        ==! Just (getImplSubmission updateSpec)
      onlyEndorsementsAreAllowed updateSpec (dropFirstValidAction fragment')
      fragmentLengthInSlots fragment' <! stableAfter env
      where
        env = unUpdateSt (firstState fragment')
    validateTransition (E (SIP (IsStably Approved)) _fragment)
                       (E (Implementation Submitted) fragment') = do
      getSubmittedImpl (firstAction fragment')
        ==! Just (getImplSubmission updateSpec)
      onlyEndorsementsAreAllowed updateSpec (dropFirstValidAction fragment')
      fragmentLengthInSlots fragment' <! stableAfter env
      where
        env = unUpdateSt (firstState fragment')
    validateTransition (E (Implementation Submitted) _fragment)
                       (E (Implementation StablySubmitted) fragment') = do
      -- Because the implementation submission might have occurred before
      -- @fragment@, we cannot validate the stability event. The fact that
      -- implementation commits can be submitted at any time (provided they are
      -- not already submitted) makes this check impossible to make with this
      -- testing strategy.
      forall (invalidActions fragment')
             (\act -> getRevealedImpl act /=! Just (getImplRevelation updateSpec)
             )
    validateTransition (E (Implementation StablySubmitted) _fragment)
                       (E (Implementation Revealed) fragment') = do
      getRevealedImpl (firstAction fragment')
        ==! Just (getImplRevelation updateSpec)
      onlyEndorsementsAreAllowed updateSpec (dropFirstValidAction fragment')
      fragmentLengthInSlots fragment' <! stableAfter env
      where
        env = unUpdateSt (firstState fragment')
    validateTransition (E (Implementation Revealed) fragment)
                       (E (Implementation StablyRevealed) fragment') = do
      validateStabilityEvent fragment fragment'
      -- The implementation voting period is open, so only implementation votes
      -- are allowed in @fragment'@.
      onlyImplVotesAreAllowed updateSpec fragment'
    validateTransition (E (Implementation StablyRevealed) fragment)
                       (E (Implementation (Is what)) fragment') = do
      validateVerdictEvent (getImpl updateSpec)
                           (getImplVoteOf (getImplId updateSpec))
                           fragment
                           (firstEvent fragment')
                           what
      onlyEndorsementsAreAllowed updateSpec fragment'
    validateTransition (E (Implementation (Is what)) fragment)
                       (E (Implementation (IsStably what')) fragment') = do
      what ==! what'
      validateStabilityEvent fragment fragment'
      onlyEndorsementsAreAllowed updateSpec fragment'
    validateTransition (E (Implementation StablyRevealed) fragment)
                       (E Queued fragment') = do
      -- If the implementation got queued, then it must have been approved.
      validateVerdictEvent (getImpl updateSpec)
                           (getImplVoteOf (getImplId updateSpec))
                           fragment
                           (firstEvent fragment')
                           Approved
      -- If the proposal was queued it must be because it cannot __yet__ follow
      -- the current version, or there is a candidate proposal with higher or
      -- the same priority.
      ( getCurrentProtocolVersion (firstState fragment')
           <! supersedesVersion (getProtocol updateSpec)
        ||!
        exists
          (candidatesAtTheBeginningOf fragment' `withIdDifferentFrom` updateSpec)
          (\protocol ->
             version protocol <! version (getProtocol updateSpec)))
      -- If the version of @updateSpec@ is queued we should not see any
      -- endorsements for this version. If another proposal with the same
      -- version was being endorsed when the proposal of @updateSpec@ entered
      -- the queue, then  the former proposal should have been canceled.
      noActionsAreAllowed updateSpec fragment'
    validateTransition (E (Implementation StablyRevealed) fragment)
                       (E BeingEndorsed fragment') = do
      validateVerdictEvent (getImpl updateSpec)
                           (getImplVoteOf (getImplId updateSpec))
                           fragment
                           (firstEvent fragment')
                           Approved
      getCurrentProtocolVersion (firstState fragment')
        ==! supersedesVersion (getProtocol updateSpec)
      getCurrentProtocolId (firstState fragment')
        ==! supersedesId (getProtocol updateSpec)
      -- All the candidate proposals different from the one being endorsed must
      -- have a version higher than the latter.
      forall
        (candidatesIn fragment' `withIdDifferentFrom` updateSpec)
        (\protocol ->
            version (getProtocol updateSpec) <! version protocol)
      onlyEndorsementsAreAllowed updateSpec fragment'
    validateTransition (E (Implementation StablyRevealed) fragment)
                       (E ActivationCanceled fragment') = do
      -- The proposal must have gotten approved, otherwise it cannot be canceled.
      validateVerdictEvent (getImpl updateSpec)
                           (getImplVoteOf (getImplId updateSpec))
                           fragment
                           (firstEvent fragment')
                           Approved
      -- The cancellation was due to an implicit cancellation.
      exists
        (candidatesAtTheBeginningOf fragment' `withIdDifferentFrom` updateSpec)
        (\protocol ->
            version (getProtocol updateSpec) ==! version protocol)
      -- The system should accept endorsements of the protocol version that
      -- canceled @updateSpec@.
      onlyEndorsementsAreAllowed updateSpec fragment'
    validateTransition (E (Implementation StablyRevealed) fragment)
                       (E ActivationUnsupported fragment') = do
      -- A revealed proposal can get approved and immediately marked as
      -- unsupported if the version it supersedes is lower than either the
      -- current version or a (potential) scheduled version.
      validateVerdictEvent (getImpl updateSpec)
                           (getImplVoteOf (getImplId updateSpec))
                           fragment
                           (firstEvent fragment')
                           Approved
      ( supersedesVersion (getProtocol updateSpec) <! getCurrentProtocolVersion st'
        ||!
        (do
          supersedesVersion (getProtocol updateSpec) ==! getCurrentProtocolVersion st'
          supersedesId (getProtocol updateSpec)      /=! getCurrentProtocolId st'
         )
       ||!
        case scheduledProtocol st' of
          Nothing ->
            failBecause
              $ "There is no scheduled protocol which might have caused the"
              <> "  proposal to become unsupported. "
          Just protocol ->
            supersedesVersion (getProtocol updateSpec) <! version protocol
       )
      onlyEndorsementsAreAllowed updateSpec fragment'
      where
        st' = unUpdateSt $ firstState fragment'
    validateTransition (E BeingEndorsed fragment)
                       (E Scheduled fragment') = do
      validateActivationVerdictEvent updateSpec
                                     fragment
                                     (firstEvent fragment')
                                     Scheduled
      supersedesVersion (getProtocol updateSpec) ==! getCurrentProtocolVersion st'
      supersedesId (getProtocol updateSpec)      ==! getCurrentProtocolId st'
      -- We should see no actions. No even endorsement of other proposals with
      -- the same version since such proposals should never make it to the
      -- endorsement period.
      noActionsAreAllowed updateSpec fragment'
      -- TODO: check that if a proposal is scheduled, then @fragment'@ cannot
      -- span an epoch boundary. This ensures that scheduled proposals are
      -- eventually applied. Required by: decentralized decision making.
      where
        st' = unUpdateSt $ firstState fragment'
    validateTransition (E BeingEndorsed fragment)
                       (E ActivationExpired fragment') = do
      validateActivationVerdictEvent updateSpec
                                     fragment
                                     (firstEvent fragment')
                                     ActivationExpired
      onlyEndorsementsAreAllowed updateSpec fragment'
    validateTransition (E BeingEndorsed fragment)
                       (E Queued fragment') = do
      -- For all endorsement periods in @fragment@ the proposal couldn't have
      -- been scheduled.
      validateActivationVerdictEvent updateSpec
                                     fragment
                                     (firstEvent fragment')
                                     Queued
      -- If the proposal went back to the queue, then in the first state of
      -- @fragment'@ there must be an endorsed proposal or one in the queue with
      -- a version lower than the one of @updateSpec@.
      exists (Update.candidateProtocols (firstState fragment'))
             (\protocol -> version protocol <! version (getProtocol updateSpec))
      onlyEndorsementsAreAllowed updateSpec fragment'
    validateTransition (E BeingEndorsed fragment)
                       (E ActivationCanceled fragment') = do
      validateActivationVerdictEvent updateSpec
                                     fragment
                                     (firstEvent fragment')
                                     ActivationCanceled
      -- At the moment cancellation can occur only if there is a proposal with
      -- the same version as @updateSpec@ in the activation queue or being
      -- endorsed.
      exists (Update.candidateProtocols (firstState fragment'))
             (\protocol -> version protocol ==! version (getProtocol updateSpec))
      -- TODO: We do not check explicit cancellations as they are not recorded
      -- by the update system.
      onlyEndorsementsAreAllowed updateSpec fragment'
    validateTransition (E Queued _fragment)
                       (E ActivationCanceled fragment') = do
      -- The activation of the update proposal must have been caused by a
      -- candidate with the same version entering the approval phase.
      exists (Update.candidateProtocols (firstState fragment'))
             (\protocol -> version protocol ==! version (getProtocol updateSpec))
      onlyEndorsementsAreAllowed updateSpec fragment'
    validateTransition (E Queued _fragment)
                       (E ActivationUnsupported fragment') = do
      -- TODO: we should check that no endorsements are allowed in this stage,
      -- however we do not identify endorsements by id yet.

      -- If the update is marked as unsupported it must be because the version
      -- it supersedes can never be adopted. This means that either the current
      -- version is greater than the version the update specification
      -- supersedes, or equal but the current version id is different from the
      -- version the update specification declares to supersede.
      ( ( supersedesVersion (getProtocol updateSpec)
           <! getCurrentProtocolVersion (firstState fragment')
        )
        ||!
        (do
            supersedesVersion (getProtocol updateSpec)
              ==! getCurrentProtocolVersion (firstState fragment')
            supersedesId (getProtocol updateSpec)
              /=! _id (getCurrentProtocol (firstState fragment'))
        ) )
      onlyEndorsementsAreAllowed updateSpec fragment'
    validateTransition (E Queued _fragment)
                       (E BeingEndorsed fragment') = do
      -- The current protocol version must be the version that the update
      -- proposal supersedes.
      supersedesId (getProtocol updateSpec) ==! _id (getCurrentProtocol st')
      -- All the queued proposals must have a version higher than this proposal.
      forall
        (candidatesIn fragment' `withIdDifferentFrom` updateSpec)
        (\protocol ->
            version (getProtocol updateSpec) <! version protocol)
      -- The system allows endorsements of proposals with the same version as
      -- @updateSpec@.
      onlyEndorsementsAreAllowed updateSpec fragment'
      where
        st' = unUpdateSt $ firstState fragment'
    validateTransition (E Scheduled fragment)
                       (E Activated fragment') = do
      noActionsAreAllowed updateSpec fragment'
      -- Proposals are activated at the beginning of an epoch.
      currentSlot st' ==! nextEpochFirstSlot st
      where
        st  = unUpdateSt $ firstState fragment
        st' = unUpdateSt $ firstState fragment'
    validateTransition (E Activated _fragment)
                       (E ActivationUnsupported fragment') = do
      noActionsAreAllowed updateSpec fragment'
      protocolVersion updateSpec <! getCurrentProtocolVersion (firstState fragment')
    validateTransition (E updateState  _)
                       (E updateState' _) =
      failBecause $  "Unexpected state change"
                  <> " from: "
                  <> cShow updateState
                  <> " to: "
                  <> cShow updateState'

    candidatesAtTheBeginningOf = Update.candidateProtocols . firstState

    -- TODO: think whether it is necessary to check the whole fragment. We will
    -- be checking the state changes to each proposal, to maybe we can argue we
    -- will detect a situation in which a proposal got at the front of the queue
    -- but it didn't start its endorsement period.
    candidatesIn = nub . concatMap Update.candidateProtocols . validStates

    withIdDifferentFrom fragment updateSpec' =
      filter ((/= _id (getProtocol updateSpec')) . _id) fragment

-- | Check the validity of a stability transition:
--
-- - the first slot of the first fragment should be __exacly__ @stableAfter@
--   slots apart from the first slot of the second fragment.
--
-- - the last slot of the first fragment should be __strictly less__ than the
--   first slot of the second fragment. This ensures that the stability event
--   was signaled by the system at the right slot.
--
validateStabilityEvent
  :: TraceFragment UpdateSUT
  -> TraceFragment UpdateSUT
  -> Assertion
validateStabilityEvent fragment fragment' = do
  stableAt env (currentSlot st0) ==! currentSlot st0'
  currentSlot stN <! currentSlot st0'
  where
    env  = st0
    st0  = unUpdateSt (firstState fragment)
    stN  = unUpdateSt (lastState fragment)
    st0' = unUpdateSt (firstState fragment')

validateVerdictEvent
  :: ( Proposal p
     , HasStakeDistribution (IState) (Id (Voter p))
     )
  => p
  -> (SUTAct UpdateSUT -> Maybe (Vote p))
  -> TraceFragment UpdateSUT
  -> TraceEvent UpdateSUT
  -> Decision
  -> Assertion
validateVerdictEvent prop extractVote votingFragment tallyEvt decision = do
  -- No verdict was reached before the last voting period.
  forall votingPeriodsWithoutVerdict
         (noVerdictCanBeReached (noMajorityConfidences Undecided))
  -- The verdict of the last voting period coincides with the given decision.
  case decisionConfidence decision of
    Nothing -> pass -- The decision does not involve majority checking.
    Just conf ->
      (stakeThreshold (adversarialStakeRatio env) (totalStake' tallyEvtSt)
       <!
       stake extractVote conf lastVotingPeriod tallyEvtSt)
  -- No verdict different from @confidence@ can be reached.
  noVerdictCanBeReached (noMajorityConfidences decision)
                        (lastVotingPeriod, lastTallySt)
  -- The number of voting periods is bounded by the maximum number of voting
  -- periods.
  length proposalVotingPeriods <=! proposalMaxVotingPeriodsI
  -- If the verdict was @Expired@ then the number of voting periods should equal
  -- the maximum number of voting periods. This means that expired proposals
  -- should have gone through the maximum number of voting periods as specified
  -- in its metadata.
  when (decision == Expired) $
    length proposalVotingPeriods ==! proposalMaxVotingPeriodsI
  -- The slot of the tally state coincides with a tally slot.
  exists
    proposalVotingPeriods
    (\(_, UpdateSt st) -> currentSlot tallyEvtSt ==! currentSlot st)
  -- No valid vote should have been rejected in a voting period.
  forall proposalVotingPeriods
         (\(votingPeriod, _) ->
            forall (invalidActions votingPeriod) noUnexpectedVoteRejection
         )
  -- No vote should have been accepted outside a voting period. It's debatable
  -- whether we should check this: if the tests cover enough cases, the they
  -- should cover the case in which the SUT accepted a vote outside the voting
  -- period causing the verdicts of the SUT and of the tests to differ. Of
  -- course this does not rule out the possibility of a vote being accepted (by
  -- the SUT) outside the voting period, but not tallied. So it might be better
  -- to be explicit about this property.
  --
  -- Also note that this implicitly tests the duration of the voting period: if
  -- the voting period allowed by the system differed with regard to the
  -- expected length, the we will either see an unexpected vote rejection, or
  -- the tally results of the tests will differ due to a vote outside the voting
  -- period being accepted and tallied by the update system, but not by our
  -- tests.
  --
  -- TODO: we leave this unchecked for now as it is (partially) checked by the
  -- tallying tests above. If time allows we might add an explicit check.
  where
    tallyEvtSt = unUpdateSt $ extractState tallyEvt
    env     = tallyEvtSt

    totalStake' st =
      totalStake @IState @(Id (Voter MockSIP)) st

    -- The voting fragment must contain the tally event so that @votingPeriods@
    -- can return all the tally states corresponding to the voting periods in
    -- @votingFragment@.
    proposalVotingPeriods =
      votingPeriods env prop (votingFragment ++ [tallyEvt])

    proposalMaxVotingPeriods  = maxVotingPeriods env
    proposalMaxVotingPeriodsI = fromIntegral proposalMaxVotingPeriods

    votingPeriodsWithoutVerdict =
      case proposalVotingPeriods of
        []  -> emptyVotingPeriodsError
        [_] -> []
        _   -> init proposalVotingPeriods

    emptyVotingPeriodsError
      = error $ "No voting periods for the proposal."
              ++ " This should not happen:"
              ++ " a verdict was reached using the votes in fragment."
              ++ " this means that we should have"
              ++ " at least one voting period."
              ++ " The voting fragment was: "
              ++ prettyShow votingFragment

    (lastVotingPeriod, lastTallySt) =
      case proposalVotingPeriods of
        [] -> emptyVotingPeriodsError
        _  -> last proposalVotingPeriods

    noVerdictCanBeReached confidences (votingPeriod, UpdateSt tallySt) =
      traverse_ (<=! stakeThreshold (adversarialStakeRatio env) (totalStake' tallySt))
        $ fmap (\conf -> stake extractVote conf votingPeriod tallySt)
               confidences

    -- Which confidences should not have the majority given the decision that
    -- was reached.
    noMajorityConfidences Rejected     = [For, Abstain]
    noMajorityConfidences WithNoQuorum = [For, Against]
    noMajorityConfidences Approved     = [Against, Abstain]
    noMajorityConfidences Expired      = [For, Against, Abstain]
    noMajorityConfidences Undecided    = [For, Against, Abstain]

    -- Which confidence requires a majority of the votes to reach the given
    -- majority decision. If the decision does not involve a majority
    -- (@Expired@, @Undecided@), then @Nothing@ is returned.
    decisionConfidence Rejected     = Just Against
    decisionConfidence WithNoQuorum = Just Abstain
    decisionConfidence Approved     = Just For
    decisionConfidence _            = Nothing

    noUnexpectedVoteRejection :: SUTAct UpdateSUT -> Assertion
    noUnexpectedVoteRejection act =
      case extractVote act of
        Nothing   -> pass
        Just vote ->
          not (signatureVerifies vote)
            `orElseShow`
            ("Unexpected rejection: if a valid vote is rejected "
            <> "during an open voting period, it can only be because "
            <> " the signature does not verify."
            )

validateActivationVerdictEvent
  :: UpdateSpec
  -> TraceFragment UpdateSUT
  -- ^ Fragment in which the update proposal is being endorsed.
  -> TraceEvent UpdateSUT
  -- ^ Event when the proposal entered the @Scheduled@ state.
  -> UpdateState
  -- ^ The update state at the activation event. Must be either @Scheduled@,
  -- @ActivationExpired@, @ActivationCanceled@, or @Queued@.
  -> Assertion
validateActivationVerdictEvent updateSpec endorsementsFragment tallyEvt updateState = do
  onlyEndorsementsAreAllowed updateSpec endorsementsFragment
  -- The proposal could not have been scheduled earlier.
  forall endorsementIntervalsWithoutVerdict notEnoughEndorsements
  -- The proposal gathered enough endorsements at the last tally state, which is
  -- in @tallyEvt@.
  case updateState of
    Scheduled          ->
      enoughEndorsements (lastEndorsementInterval, lastTallySt, lastThreshold)
    ActivationExpired  ->
      notEnoughEndorsements (lastEndorsementInterval, lastTallySt, lastThreshold)
    ActivationCanceled ->
      when (not (null proposalEndorsementIntervals)) $
        notEnoughEndorsements (lastEndorsementInterval, lastTallySt, lastThreshold)
    Queued             ->
      when (not (null proposalEndorsementIntervals)) $
        notEnoughEndorsements (lastEndorsementInterval, lastTallySt, lastThreshold)
    _                  -> error $ "validateActivationVerdictEvent:"
                                ++ " the update state must be either"
                                ++ "`Scheduled`, `ActivationExpired`,"
                                ++ " `ActivationCanceled`, or `Queued`."
  -- No valid endorsement is rejected.
  forall proposalEndorsementIntervals
         (\(endorsementInterval, _, _) ->
            forall (invalidActions endorsementInterval)
                   noUnexpectedEndorsementRejection
         )
  -- The number of endorsement periods is bounded by the maximum number of
  -- endorsement intervals.
  --
  -- TODO: the maximum number of endorsement intervals is not configurable yet.
  length proposalEndorsementIntervals <=! 2
  -- If we have a last slot in which tally took place, then the tally state
  -- coincides with a tally slot. In other words, scheduling or expiration of
  -- proposals can occur only at a tally point.
  when (updateState == Scheduled || updateState == ActivationExpired) $
    currentSlot tallyEvtSt ==! currentSlot lastTallySt
  where
    tallyEvtSt = unUpdateSt $ extractState tallyEvt

    endorsementIntervalsWithoutVerdict =
      case proposalEndorsementIntervals of
        []  -> [] -- There are no endorsement intervals without a verdict.
        _:_ -> init proposalEndorsementIntervals

    emptyIntervalsError = error $ "Expected at least one endorsement interval"

    (lastEndorsementInterval, lastTallySt, lastThreshold) =
      case proposalEndorsementIntervals of
        []  -> emptyIntervalsError
        _:_ -> last proposalEndorsementIntervals

    proposalEndorsementIntervals =
      endorsementIntervals (endorsementsFragment ++ [tallyEvt])

    enoughEndorsements (endorsementInterval, tallySt, threshold) =
      threshold <=! endorsingStake updateSpec endorsementInterval tallySt

    -- Due to the way we encoded assertions, we cannot define a useful negation
    -- operator on assertions, since it will not be capable of giving meaningful
    -- information on why an assertion failed.
    notEnoughEndorsements (endorsementInterval, tallySt, threshold) =
      endorsingStake updateSpec endorsementInterval tallySt <! threshold

    noUnexpectedEndorsementRejection act =
      (getEndorsedVersion act ?/= protocolVersion updateSpec)
        `orElseShow` "Unexpected endorsement rejection."

-- | Calculate the stake that voted for a given proposal with the given confidence.
--
-- The proposal is specified by means of the extraction function. If the SUT
-- action contains a vote for the relevant proposal, the extraction function
-- returns the vote author and confidence. The confidence is used to calculate
-- the last confidence seen in the fragment per author.
stake
  :: forall p
   . ( Proposal p
     , HasStakeDistribution (IState) (Id (Voter p))
     )
  => (SUTAct UpdateSUT -> Maybe (Vote p))
  -> Confidence
  -> TraceFragment UpdateSUT
  -> IState
  -> Stake
stake extractVoteAndConfidence vConfidence votingPeriod tallySt  =
  stakeOfKeys' voters
               (stakeDistribution @(IState)
                                  @(Id (Voter p))
                                  tallySt
               )
  where
    voters = Set.fromList
           $ fmap voter
           $ filter ((== vConfidence) . confidence)
           $ removeOverridenVotes
           $ catMaybes
           $ fmap extractVoteAndConfidence
           $ validActions
           $ votingPeriod

    -- Remove the votes by the same key that were overridden.
    removeOverridenVotes =
      -- We reverse the list first since we want the last vote to take
      -- precedence over earlier votes.
      nubBy (\vote0 vote1 -> voter vote0 == voter vote1) . reverse

-- | Compute the stake that endorsed the proposal in the trace fragment, using
-- the stake distribution at the tally state.
endorsingStake
  :: UpdateSpec
  -> TraceFragment UpdateSUT
  -> IState
  -> Stake
endorsingStake updateSpec endorsementInterval tallySt =
  stakeOfKeys' endorsements (stakeDistribution tallySt)
  where
    endorsements
      = Set.fromList
      $ catMaybes
      $ fmap extractEndorsement
      $ validActions endorsementInterval
      where
        extractEndorsement act = do
          e <- getEndorsement act
          if Update.endorsedVersion e == protocolVersion updateSpec
          then Just $ Update.endorserId e
          else Nothing

-- | Return the voting periods of an update specification. A voting period is a
-- pair of the trace fragment in which the voting period is open, and the state
-- at which tally should take place for that fragment.
--
-- A voting period is determined by the voting period followed by the
-- waiting-for-stabilization period. In the last state of this stabilization
-- period tallying takes place.
--
-- We assume that the given fragment contains the last tally slot, and the
-- fragment begins at the earliest slot in which the proposal was stably
-- revealed.
votingPeriods
  :: ( TracksSlotTime env
     , HasVotingPeriodsCap env
     , Proposal p)
  => env
  -- ^ Environment that provides the stability parameter.
  -> p
  -- ^ Proposal for which the voting periods are computed.
  -> TraceFragment UpdateSUT
  -- ^ Trace fragment for which the voting periods are calculated.
  -> [(TraceFragment UpdateSUT, SUTSt UpdateSUT)]
votingPeriods env prop fragment =
  go fragment
     (votingPeriodStartSlots env
                             prop
                             (revealedAt env fragment)
                             mvp
     )
  where
    mvp = maxVotingPeriods env

    go
      :: TraceFragment UpdateSUT
      -> [SlotNo]
      -> [(TraceFragment UpdateSUT, SUTSt UpdateSUT)]
    go []        _            = []
    go _         []           = []
    go fragment' (vpStart:xs) =
      case  dropWhile (slotIs (< vpStart)) fragment' of
        [] ->
          -- This describes a situation in which there's no slot in @fragment'@
          -- at which the voting period starts, this means that there are no
          -- voting periods left.
          --
          -- Since the fragment passed to this function must include the tally
          -- state, this situation should not arise.
          error "The last state of a fragment must include the tally state"
        vs ->
          -- There is a slot in @fragment'@ where the voting period can start.
          -- Note that the voting period can still be empty if the voting period
          -- duration is 0.
          let
            vpEnd = vpStart + votingPeriodDuration prop
            (votingOpenFragment, fragmentAfterVotingPeriodEnd)
              = span (slotIs (< vpEnd)) vs
          in case dropWhile (slotIs (/= vpEnd + stableAfter env))
                                 fragmentAfterVotingPeriodEnd     of
               tallyEvt:rest ->
                 (votingOpenFragment, extractState tallyEvt): go rest xs
               []                          ->
                 error $ "There should be a tally slot in the fragment: \n"
                       ++ prettyShow fragmentAfterVotingPeriodEnd

revealedAt :: TracksSlotTime env => env -> TraceFragment UpdateSUT -> SlotNo
revealedAt env fragment =
  currentSlot (unUpdateSt (firstState fragment)) - stableAfter env

-- | Compute the endorsement intervals, assuming the endorsement period started
-- at the first slot of the fragment. The endorsement intervals are computed
-- till there are no more cutoff states in the trace fragment. See function
-- @cutoffAndTallySlots@ for a definition of cutoff slots.
--
-- PRECONDITION:
--
-- - The trace fragment cannot be empty.
--
endorsementIntervals
  :: TraceFragment UpdateSUT
  -> [(TraceFragment UpdateSUT, IState, Stake)]
endorsementIntervals period =
  assert preconditionsHold
  [ ( takeWhile
        (slotIs (< cutoffSlot))
        period
    , tallyState
    , adjustedAdoptionThreshold tallyState
    )
  | (cutoffSlot, tallySlot) <- cutoffAndTallySlots startSt endSt
  , let tallyState = unUpdateSt $ period `firstStateWithSlot` tallySlot
  ]
  where
    preconditionsHold = do
      not (null period) `orElseShow` "A trace fragment cannot be empty when computing the endorsement intervals."

    startSt = unUpdateSt $ firstState period

    endSt = unUpdateSt $ lastState period

    adjustedAdoptionThreshold st =
      if endOfSafetyLag startSt <= currentSlot st + stableAfter st
      then
        ceiling $ 0.51 * (fromIntegral totalStake' :: Double)
      else
        stakeThreshold (adversarialStakeRatio st) totalStake'
      where
        totalStake' =
          totalStake @(IState)
                     @(EndorserId (Protocol MockImpl))
                     st

-- | Return the first state that has a slot equal to the given one. If no such
-- state exists an error is thrown.
firstStateWithSlot
  :: IsTraceFragment f UpdateSUT => f -> SlotNo -> SUTSt UpdateSUT
firstStateWithSlot trace slot =
  case dropWhile ((/= slot) . currentSlot . unUpdateSt) $ validStates trace of
    st:_ -> st
    []   -> error $ "No state with slot " ++ show slot ++ " was found in the trace."


-- | Determine the slots at which endorsements stop being counted for each epoch
-- (i.e. cutoff slots) between and including the start and end state.
--
-- Henceforth we refer to the slot of the start state as __start slot__ and the
-- slot of the end state as the __end slot__.
--
-- A cutoff slot occurs @2 * stableAfter@ slots before the end of an epoch,
-- where @stableAfter@ is maximum number of slots the ledger can rollback.
--
-- For each first slot of an epoch @s_i@ that lies in the interval
--
-- > [start slot, end slot]
--
-- we calculate the cutoff point of the previous epoch as:
--
-- > s_i - 2 * stableAfter
--
-- This function returns all the cutoff slots that lie in
--
-- > [start slot, end slot]
--
-- PRECONDITION:
--
-- - the current slot of the initial state has to be less or equal than the
--   current slot of the final state.
--
-- - the number of slots per epoch is greater than twice the value of
--   @stableAfter@.
cutoffSlots
  :: TracksSlotTime st
  => st
  -- ^ Initial state from which to start looking for cutoff points.
  -> st
  -- ^ Final state at which we stop looking for cutoff points. The final state
  -- is considered as a potential cutoff point.
  -> [SlotNo]
cutoffSlots startSt endSt
  = assert preconditionsHold
  $ dropWhile (< currentSlot startSt)
  $ takeWhile (<= currentSlot endSt)
  $ [ slot i | i <- [0..] ]
  where
    preconditionsHold = do
      currentSlot startSt     <=! currentSlot endSt
      2 * stableAfter startSt <!  slotsPerEpoch startSt

    slot j = nextEpochFirstSlot startSt
           + j * slotsPerEpoch startSt
           - 2 * stableAfter startSt

-- | Determine the cuttoff and tally slots for each epoch between and including
-- the start and end state.
--
-- A tally slot occurs @stableAfter@ slots before the end of an epoch, or
-- equivalently @stableAfter@ slots after the cutoff slot of an epoch.
--
-- This function returns all the cutoff and tally slots that lie in:
--
-- > [start slot, end slot]
--
-- See function @cutoffSlots@ for more details.
--
-- PRECONDITION:
--
-- - the current slot of the initial state has to be less or equal than the
--   current slot of the final state.
--
-- - the number of slots per epoch is greater than twice the value of
--   @stableAfter@.
--
-- POSTCONDITION:
--
-- - All the returned slots are in the range [start slot, end slot]
cutoffAndTallySlots
  :: TracksSlotTime st
  => st
  -- ^ Initial state from which to start looking for cutoff and tally points.
  -> st
  -- ^ Final state at which we stop looking for cutoff and tally points. The
  -- final state is considered as a potential cutoff point.
  -> [(SlotNo, SlotNo)]
cutoffAndTallySlots startSt endSt
  = assertAndReturn postconditionsHold
  $ takeWhile ((<= currentSlot endSt) . snd)
  $ [ (cutoffSlot, cutoffSlot + stableAfter startSt)
    | cutoffSlot <- cutoffSlots startSt endSt
    ]
  where
    postconditionsHold slotPairs =
      forall slotPairs (\(s, t) -> do
                           currentSlot startSt <=! s
                           currentSlot startSt <=! t
                           s <=! currentSlot endSt
                           t <=! currentSlot endSt
                       )

-- TODO: we should check that actions with invalid signatures are rejected.
-- invalidSignaturesAreRejected :: _
-- invalidSignaturesAreRejected = undefined

-- Assert that no actions are allowed.
--
-- Note that we can only relate endorsements to update specifications only by
-- the protocol version of the endorsements, so use this function in case no
-- endorsements of @updateSpec@ protocol version are allowed.
noActionsAreAllowed
  :: UpdateSpec
  -> TraceFragment UpdateSUT
  -> Assertion
noActionsAreAllowed updateSpec fragment =
  forall (validActions fragment)
         (\act -> do
             -- No SIP actions are allowed.
             getSubmittedSIP act /=! Just (getSIPSubmission updateSpec)
             getRevealedSIP  act /=! Just (getSIPRevelation updateSpec)
             getVotedSIP     act /=! Just (getSIPId updateSpec)
             -- No implementation actions are allowed
             getSubmittedImpl act /=! Just (getImplSubmission updateSpec)
             getRevealedImpl  act /=! Just (getImplRevelation updateSpec)
             getVotedImpl     act /=! Just (getImplId updateSpec)
             -- No endorsements are allowed with .
             getEndorsedVersion act /=! Just (protocolVersion updateSpec)
         )

onlyEndorsementsAreAllowed
  :: UpdateSpec
  -> TraceFragment UpdateSUT
  -> Assertion
onlyEndorsementsAreAllowed updateSpec fragment =
  forall (validActions fragment)
         (\act -> do
             -- No SIP actions are allowed.
             getSubmittedSIP act /=! Just (getSIPSubmission updateSpec)
             getRevealedSIP  act /=! Just (getSIPRevelation updateSpec)
             getVotedSIP     act /=! Just (getSIPId updateSpec)
             -- No implementation actions are allowed
             getSubmittedImpl act /=! Just (getImplSubmission updateSpec)
             getRevealedImpl  act /=! Just (getImplRevelation updateSpec)
             getVotedImpl     act /=! Just (getImplId updateSpec)
         )

onlyImplementationSubmissionAllowed
  :: UpdateSpec
  -> TraceFragment UpdateSUT
  -> Assertion
onlyImplementationSubmissionAllowed updateSpec fragment =
  forall (validActions fragment)
         (\act -> do
             -- No SIP actions are allowed.
             getSubmittedSIP act /=! Just (getSIPSubmission updateSpec)
             getRevealedSIP  act /=! Just (getSIPRevelation updateSpec)
             getVotedSIP     act /=! Just (getSIPId updateSpec)
             -- Only an implementation submission is allowed: the system has no
             -- way of checking to which SIP the commit corresponds to, since
             -- such commit is basically a hash.
             getRevealedImpl act /=! Just (getImplRevelation updateSpec)
             getVotedImpl    act /=! Just (getImplId updateSpec)
             -- No endorsements should be allowed. However we do not identify
             -- endorsements by proposal id, so we cannot do check
             -- anything here.
         )

onlyImplVotesAreAllowed
  :: UpdateSpec
  -> TraceFragment UpdateSUT
  -> Assertion
onlyImplVotesAreAllowed updateSpec fragment =
  forall (validActions fragment)
         (\act -> do
             -- No SIP actions are allowed.
             getSubmittedSIP act /=! Just (getSIPSubmission updateSpec)
             getRevealedSIP  act /=! Just (getSIPRevelation updateSpec)
             getVotedSIP     act /=! Just (getSIPId updateSpec)
             -- Only implementation votes are allowed.
             getSubmittedImpl act /=! Just (getImplSubmission updateSpec)
             getRevealedImpl  act /=! Just (getImplRevelation updateSpec)
             -- No endorsements should be allowed. However we do not identify
             -- endorsements by proposal id, so we cannot do check
             -- anything here.
         )

onlyImplementationSubmissionOrSIPVotesAllowed
  :: UpdateSpec
  -> TraceFragment UpdateSUT
  -> Assertion
onlyImplementationSubmissionOrSIPVotesAllowed updateSpec fragment =
  forall (validActions fragment)
         (\act -> do
             -- No SIP actions are allowed.
             getSubmittedSIP act /=! Just (getSIPSubmission updateSpec)
             getRevealedSIP  act /=! Just (getSIPRevelation updateSpec)
             -- Only an implementation submission is allowed: the system has no
             -- way of checking to which SIP the commit corresponds to, since
             -- such commit is basically a hash.
             getRevealedImpl act /=! Just (getImplRevelation updateSpec)
             getVotedImpl    act /=! Just (getImplId updateSpec)
             -- No endorsements should be allowed. However we do not identify
             -- endorsements by proposal id, so we cannot do check
             -- anything here.
         )

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent _ [] = []
mapAdjacent f xs = zipWith f xs (tail xs)

-- | Compute the difference between the first and last slot of the fragment.
fragmentLengthInSlots :: TraceFragment UpdateSUT -> SlotNo
fragmentLengthInSlots fragment =
  currentSlot stN - currentSlot st0
  where
    st0  = unUpdateSt (firstState fragment)
    stN  = unUpdateSt (lastState fragment)

-- | Compute the slots at which voting periods start.
--
votingPeriodStartSlots
  :: (TracksSlotTime env, Proposal p)
  => env
  -- ^ Environment that provides the stability parameter.
  -> p
  -- ^ Proposal for which the start of the voting periods are computed. The
  -- proposal determines the maximum number of voting periods.
  -> SlotNo
  -- ^ Slot in which the proposal was revealed.
  -> VotingPeriod
  -- ^ Number of voting periods
  -> [SlotNo]
votingPeriodStartSlots env prop revelationSlot propMaxVotingPeriods =
  [ revelationSlot + periodOffset p + stableAfter env
  | p <- [0 .. (fromInteger (toInteger propMaxVotingPeriods) - 1)]
  ]
  where
    periodOffset p = p * (stableAfter env + votingPeriodDuration prop)
