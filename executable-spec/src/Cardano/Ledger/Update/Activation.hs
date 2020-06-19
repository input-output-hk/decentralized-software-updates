{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Update.Activation
  ( State
    -- * Interface functions
  , initialState
  , tick
  , transferApprovals
  , endorse
    -- ** Endorsement construction
  , Endorsement (Endorsement, endorserId, endorsedVersion)
    -- ** Error reporting
  , Error
  , HasActivationError (getActivationError)
  , endorsementErrorGivenProtocolVersion
  , endorsementErrorEndorsedProposal
  , endorsedVersionError
    -- * Update state query operations
  , State.HasActivationState (getActivationState)
  , State.getCurrentProtocol
  , State.getCurrentProtocolId
  , State.getCurrentProtocolVersion
  , State.currentProtocol
  , State.isQueued
  , State.queuedProtocols
  , State.candidateProtocols
  , State.isBeingEndorsed
  , State.isScheduled
  , State.scheduledProtocol
  , State.scheduledProtocolVersion
  , State.isTheCurrentVersion
  , State.candidateEndOfSafetyLag
  , State.isDiscardedDueToBeing
  , State.endOfSafetyLag
  , State.Reason (Expired, Canceled, Unsupported)
  )
where

import           Control.Arrow (right)
import           Data.Function ((&))
import           Data.List (foldl')
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Cardano.Slotting.Slot (SlotNo)

import           Cardano.Ledger.Update.Activation.State
                     (MaybeAnEndorsedProposal, State, cEndOfSafetyLag,
                     cEndorsements)
import           Cardano.Ledger.Update.Env.HasAdversarialStakeRatio
                     (HasAdversarialStakeRatio, adversarialStakeRatio)
import           Cardano.Ledger.Update.Env.HasStakeDistribution
                     (HasStakeDistribution, stakeDistribution, totalStake)
import           Cardano.Ledger.Update.Env.StakeDistribution (stakeOfKeys',
                     stakeThreshold)
import           Cardano.Ledger.Update.Env.TracksSlotTime (TracksSlotTime,
                     currentSlot, epochFirstSlot, nextEpochFirstSlot,
                     slotsPerEpoch, stableAfter)

import qualified Cardano.Ledger.Update.Activation.State as State
import qualified Cardano.Ledger.Update.Approval as Approval

import           Cardano.Ledger.Update.Proposal

import           Cardano.Ledger.Assert


-- | Check 'State' invariants. Note that invariants 0 and 1 are satisfied by
-- construction.
--
checkInvariants
  :: (HasCallStack, Implementation sip impl)
  => State sip impl
  -> State sip impl
checkInvariants st = assert checks st
  where
    checks = do
      forall (State.queuedProtocols st)
             ((/=! State.currentProtocolVersion st) . version)
      forall (State.queuedProtocols st)
             ((/=?! State.endorsedProtocolVersion st) . version)

      State.currentProtocolVersion st <?!  State.endorsedProtocolVersion st
      State.currentProtocolVersion st ==?! State.endorsedSupersedesVersion st

      forall (State.queuedProtocols st)
             ((State.currentProtocolVersion st <=!) . supersedesVersion)

      forall (State.queuedProtocols st)
             ((State.endorsedProtocolVersion st ?<!) . version)

      -- If the last applied slot is greater than the end of the safety lag the
      -- candidate is either expired or adopted.
      --
      -- Versions are adopted in the first slot of a new epoch.
      State.lastAppliedSlot st ?<=?! State.candidateEndOfSafetyLag st

--------------------------------------------------------------------------------
-- Interface functions
--------------------------------------------------------------------------------

initialState
  :: (HasCallStack, Implementation sip impl)
  => Protocol impl
  -- ^ Initial protocol. This determines the current version.
  -> State sip impl
initialState = checkInvariants . State.initialState

-- | Register a slot tick. A slot tick might cause several changes in the
-- activation state. These changes are described below.
--
-- If the slot is the first slot of the current epoch, then the scheduled update
-- proposal (@i@) becomes adopted, and its version becomes the current version.
--
-- Adopting a scheduled proposal causes the cancellation of all proposals that
-- can no longer follow. We know that a version can never follow if it is less
-- than the current version, because versions always increase strictly.
--
-- If we are at the slot at which the tally must take place and there is a
-- candidate @i@, then the endorsements for this candidate are counted. If it
-- has enough endorsements it will become scheduled for adoption at the
-- beginning of the next epoch.
--
-- A slot tick can expire the candidate when it does not reach enough
-- endorsements at its last tally phase (the number of tally phases is
-- determined by the length of the safety lag).
--
-- Expiring a candidate proposal or adopting a scheduled proposal might cause
-- the next proposal @j@ in the queue (if any) to become active. For this to
-- happen, proposal @j@ must supersede the (new) current version.
--
tick
  :: forall env sip impl
   . ( HasCallStack
     , TracksSlotTime env
     , HasStakeDistribution env (EndorserId (Protocol impl))
     , HasAdversarialStakeRatio env
     , Implementation sip impl
     )
  => env
  -> State sip impl
  -> State sip impl
tick env st
  = checkPreCondition st
  & tryTransferEndorsements
  & tryActivate
  & tryTally
  & tryFindNewCandidate env -- We do this at the end, since activation or
                            -- expiration cause the next proposal to be put in the
                            -- queue.
  -- TODO: we might want to take a stake distribution snapshot.
  --  & tryToTakeStakeDistributionSnapshot
  & State.tickLastAppliedSlot (currentSlot env)
  where
    checkPreCondition =
      assert $ (currentSlot env - 1) ==?! State.lastAppliedSlot st

    tryTransferEndorsements st'
      | currentSlot env == epochFirstSlot env = State.transferEndorsements st'
      | otherwise                             = st'

    -- | Activate an eventual candidate protocol version
    --
    -- TODO: discuss this with Edsko.
    tryActivate st'
      | currentSlot env == epochFirstSlot env = State.activate st'
      | otherwise                             = st'

    tryTally st' =
      case State.endorsedProposal st' of
        State.Candidate { cEndorsements, cEndOfSafetyLag }
          | atTallyPoint && candidateHasEnough cEndorsements cEndOfSafetyLag
            -> State.scheduleCandidate st'
          | (atTallyPoint || pastTallyPoint)
            && noNextEndorsementPeriod cEndOfSafetyLag env
            ->
             -- If the a proposal became candidate after the cutoff period, but
             -- has no next endorsement period it should be expired, since no
             -- endorsements can be considered in the epoch, and there is no
             -- next endorsement period.
             State.discardCandidate State.Expired st'
        _   -> st'
      where
        atTallyPoint = currentSlot env == tallySlot env

        pastTallyPoint = tallySlot env < currentSlot env

        candidateHasEnough endorsements endOfSafetyLag =
          adjustedAdoptionThreshold <= totalEndorsements
          where
            adjustedAdoptionThreshold
              | noNextEndorsementPeriod endOfSafetyLag env
              = ceiling $ 0.51 * (fromIntegral endorsersTotalStake :: Double)
              | otherwise
              = stakeThreshold (adversarialStakeRatio env)
                               endorsersTotalStake
              where
                endorsersTotalStake =
                  totalStake @env @(EndorserId (Protocol impl)) env

            totalEndorsements =
              stakeOfKeys' (State.thisEpochEndorsements endorsements)
                           (stakeDistribution env)


noNextEndorsementPeriod
  :: TracksSlotTime env
  => SlotNo
  -> env
  -> Bool
noNextEndorsementPeriod endOfSafetyLag env =
  endOfSafetyLag <= nextEpochFirstSlot env

-- | The approval of an proposal causes several changes in the activation state.
-- These changes are described below.
--
-- A cancellation-update proposal @j@ might cancel a proposal @i@ in the
-- activation state. Scheduled proposals cannot be canceled.
--
-- If a new new update proposal @j@ is approved, then we have to check whether
-- it can follow the current version or any scheduled version. If not, the
-- proposal gets immediately discarded.
--
-- A new update proposal that can follow the current and an eventual scheduled
-- version might cause an eventual candidate proposal to be:
--
-- - canceled, if @j@ has the same version as the candidate.
-- - put back into the queue, if @j@ has a lower version.
--
-- After adding a new candidate to the queue, and resolving its conflicts with a
-- potential candidate, we try to find a new candidate if there is none.
--
-- If a newly approved proposal @j@ is a software update, then it becomes
-- immediately activated. Software updates require no endorsements, since they
-- have no impact on the protocol.
--
-- Postconditions:
--
-- 1. The scheduled proposal is not changed (nothing can cancel a scheduled
-- proposal).
--
-- 2. There are no proposals in the intersection of the approval and activation
-- state
--
transferApprovals
  :: ( HasCallStack
     , TracksSlotTime env
     , Implementation sip impl
     )
  => env
  -> Approval.State impl
  -> State sip impl
  -> (Approval.State impl, State sip impl)
transferApprovals env approvalSt st = (approvalSt', checkInvariants st')
  where
    (approved, approvalSt') = Approval.removeApproved approvalSt
    -- Cancellations are applied at the end, in case a proposal gets approved
    -- and canceled in the same slot. This shouldn't happen in practice, since
    -- the experts should decide on one or the other instead of a approving the
    -- update and its cancellation at the same time.
    st'                     = foldl' dispatch st approved

    -- TODO: Cancel the proposals in the end.
    --
    -- TODO: the order in which we transfer the approvals might alter the resulting
    -- state. We should think whether this is a problem.
    dispatch st'' aProposal =
      case implementationType aProposal of
        Cancellation { toCancel } -> foldl' cancel st'' toCancel
        Protocol protocol         -> addProtocolUpdateProposal env st'' protocol
        Application application   -> updateApplication st'' application


-- TODO: if the endorsements come in transactions we need to prevent endorsement
-- replays, and we need the endorsement to be signed.
--
-- TODO: check: if endorsements come into blocks, since only block issuers can
-- endorse, then we shouldn't worry about replay attacks and forgery.
data Endorsement sip impl =
  Endorsement
  { endorserId       :: !(EndorserId (Protocol impl))
  , endorsedVersion  :: !(Version (Protocol impl))
  } deriving (Generic)

deriving instance Implementation sip impl => Show (Endorsement sip impl)

data Error sip impl =
  ProtocolVersionIsNotACandidate
  { endorsementErrorGivenProtocolVersion :: !(Version (Protocol impl))
  , endorsementErrorEndorsedProposal     :: !(MaybeAnEndorsedProposal sip impl)
  } deriving (Generic)

deriving instance Implementation sip impl => Show (Error sip impl)

-- | Register an endorsement.
--
-- PRECONDITION: the number of slots per-epoch must be greater than twice
-- stability window (i.e. the number of slots after which events are stable on
-- the chain). This ensures that the cutoff period for endorsements lies within
-- the epoch in which the endorsement took place.
endorse
  :: ( HasCallStack
     , TracksSlotTime env
     , Implementation sip impl
     )
  => env
  -> Endorsement sip impl
  -> State sip impl
  -> Either (Error sip impl) (State sip impl)
endorse env Endorsement {endorserId, endorsedVersion} st
  = assert ((2 * stableAfter env) <! slotsPerEpoch env)
  $ right checkInvariants
  $ case State.candidateProtocol st of
    Just cProtocol
      | version cProtocol == endorsedVersion
        -> Right $ endorseUnchecked endorserId env st
    _   -> Left  $ ProtocolVersionIsNotACandidate
                   { endorsementErrorGivenProtocolVersion = endorsedVersion
                   , endorsementErrorEndorsedProposal     = State.endorsedProposal st
                   }

endorseUnchecked
  :: ( TracksSlotTime env
     , Implementation sip impl
     )
  => EndorserId (Protocol impl)
  -> env
  -> State sip impl
  -> State sip impl
endorseUnchecked endorserId env st =
  if currentSlot env < cutOffSlot env
  then State.endorseInThisEpoch endorserId st
  else State.endorseInNextEpoch endorserId st

cutOffSlot :: TracksSlotTime env => env -> SlotNo
cutOffSlot env = nextEpochFirstSlot env - (2 * (stableAfter env))

tallySlot :: TracksSlotTime env => env -> SlotNo
tallySlot env = nextEpochFirstSlot env - (stableAfter env)

--------------------------------------------------------------------------------
-- Auxiliary functions
--------------------------------------------------------------------------------

-- | Add a new proposal to the activation phase.
--
-- A new proposal can:
--
-- - Become the new candidate if the following two conditions hold:
--
--   - the new proposal has the lowest version among the proposals in the
--     activation phase.
--
--   - there is no scheduled proposal.
--
--   If the new proposal replaces a candidate, but it does not replaces it, the
--   old candidate is put back in the queue.
--
-- - Cause the cancellation of an existing proposal if the new proposal has the
--   same version as a non-scheduled proposal.
--
-- - Be immediately discarded if there is a scheduled proposal that has a
--   version that is higher than the version the new proposal supersedes. This
--   is because the version the new proposal supersedes will never be adopted.
--
addProtocolUpdateProposal
  :: ( TracksSlotTime env
     , Implementation sip impl
     )
  => env
  -> State sip impl
  -> Protocol impl
  -> State sip impl
addProtocolUpdateProposal env st protocol
  | cannotFollowCurrentVersion || cannotFollowScheduledProposal =
    State.obsoleteProposal protocol st
  | otherwise
  = State.enqueue protocol st
  & resolveConflictWithCandidate
  & tryFindNewCandidate env
  where
    protocolVersion                  = version protocol
    protocolSupersedesVersion        =
      supersedesVersion protocol
    cannotFollowCurrentVersion       =
      protocolSupersedesVersion < State.getCurrentProtocolVersion st
      ||
      (protocolSupersedesVersion == State.getCurrentProtocolVersion st
      && supersedesId protocol   /= State.getCurrentProtocolId st
      )
    cannotFollowScheduledProposal    =
      case State.scheduledProtocol st of
        Just sProtocol -> protocolSupersedesVersion < version sProtocol
        _              -> False
    resolveConflictWithCandidate st' =
      case State.candidateProtocol st' of
        Just cProtocol
          | protocolVersion == version cProtocol
            -> State.discardCandidate State.Canceled st'
          | protocolVersion < version cProtocol
            -> State.reEnqueueCandidate st'
        _   -> st'

-- | If there is candidate or scheduled proposal, determine whether there is a
-- proposal in the queue that should become the next candidate.
tryFindNewCandidate
  :: ( TracksSlotTime env
     , Implementation sip impl
     )
  => env
  -> State sip impl
  -> State sip impl
tryFindNewCandidate env st =
  case State.endorsedProtocol st of
    Nothing -> State.findANewCandidate env st
      -- TODO: we need to make the safety lag configurable. The safety lag is an
      -- attribute exclusive of protocol updates, so this should be part of the
      -- 'Protocol' type.
    _       -> st

-- | Cancel an update proposal.
--
-- Only scheduled proposals cannot be canceled.
cancel
  :: (Implementation sip impl)
  => State sip impl
  -> ProtocolId impl
  -> State sip impl
cancel st toCancel
  = State.discardProposalsThatDoNotSatisfy ((/= toCancel) . _id) State.Canceled st
  & cancelEventualCandidate
  where
    cancelEventualCandidate st' =
      case State.candidateProtocol st' of
        Just cProtocol
          | _id cProtocol == toCancel -> State.discardCandidate State.Canceled st'
        _ -> st'

updateApplication
  :: (Implementation sip impl)
  => State sip impl
  -> Application impl
  -> State sip impl
updateApplication st application = State.addApplication application st

--------------------------------------------------------------------------------
-- Update state query operations
--------------------------------------------------------------------------------

class HasActivationError err sip impl | err -> sip, err -> impl where
  getActivationError :: err -> Maybe (Error sip impl)

instance HasActivationError (Error sip impl) sip impl where
  getActivationError = Just . id

endorsedVersionError
  :: HasActivationError err sip impl => err -> Maybe (Version (Protocol impl))
endorsedVersionError
  = fmap endorsementErrorGivenProtocolVersion . getActivationError
