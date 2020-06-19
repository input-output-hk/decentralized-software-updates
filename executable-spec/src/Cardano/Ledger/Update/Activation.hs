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
  , HasActivationState (getActivationState)
  , getCurrentProtocol
  , getCurrentProtocolVersion
  , currentProtocol
  , isQueued
  , isBeingEndorsed
  , isScheduled
  , isTheCurrentVersion
  , candidateEndOfSafetyLag
  , isDiscardedDueToBeing
  , Reason (Expired, Canceled, Obsoleted, Unsupported)
  )
where

import           Data.Function ((&))
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)

import           Cardano.Slotting.Slot (SlotNo)

import           Cardano.Ledger.Update.Env.HasAdversarialStakeRatio
                     (HasAdversarialStakeRatio, adversarialStakeRatio)
import           Cardano.Ledger.Update.Env.HasStakeDistribution
                     (HasStakeDistribution, stakeDistribution, totalStake)
import           Cardano.Ledger.Update.Env.StakeDistribution (Stake,
                     stakeOfKeys', stakeThreshold)
import           Cardano.Ledger.Update.Env.TracksSlotTime (TracksSlotTime,
                     currentSlot, epochFirstSlot, nextEpochFirstSlot,
                     slotsPerEpoch, stableAfter)

import qualified Cardano.Ledger.Update.Approval as Approval

import           Cardano.Ledger.Update.Proposal

import           Cardano.Ledger.Assert

-- | Invariants:
--
-- 0. A proposal cannot be a candidate and being scheduled at the same time.
--
-- 1. There are endorsements only if there is a candidate proposal.
--
-- 2. The queue does not contain the current version.
--
-- 3. The queue does not contain the endorsed version.
--
-- 4. The candidate version is higher than the current version.
--
-- 5. The candidate can always follow the current version.
--
-- 6. All the proposals in the queue supersede a version greater or equal than
-- the current version.
--
-- 7. All the proposals in the queue have a version higher than the potential
-- endorsed version.
--
data State sip impl =
  State
  { endorsedProposal   :: !(MaybeAnEndorsedProposal sip impl)
  , currentProtocol    :: !(Protocol impl)
  , activationQueue    :: !(Map (Version (Protocol impl)) (Protocol impl))
    -- ^ The activation queue maintains a priority queue of protocol versions.
    -- The lower the version the higher the priority. Duplicated versions are
    -- not allowed. Inserting a duplicated version replaces the (implementation
    -- and hash of the) older version.
  , applicationUpdates :: ![Application impl]
    -- ^ Approved application updates.
    --
    -- TODO: this is a list for now. If needed this can be turned into a set, or
    -- a map indexed by application names.
  , lastAppliedSlot    :: !(Maybe SlotNo)
    -- ^ We require that the 'tick' function is applied without skipping any
    -- slot. Therefore we record the last applied slot to check this
    -- pre-condition in 'tick'.
    --
    -- TODO: we might want to move last applied slot and invariant checking to
    -- the update interface state. Otherwise we'll have to replicate this in all
    -- the three phases (in which we can't miss a slot tick).

  , discarded          :: !(Map (Protocol impl) Reason)
    -- TODO: the stakepoolsDistribution snapshot should probably go here.
  } deriving (Generic)

deriving instance Implementation sip impl => Show (State sip impl)

-- | Reason why an implementation did not get adopted and was discarded.
data Reason
  = Expired
  -- ^ The candidate did not meet the adoption threshold at the end of the
  -- safety lag.
  | Canceled
  -- ^ The implementation was explicitly canceled by a cancellation proposal.
  | Obsoleted
  -- ^ The implementation was canceled by another with the same version.
  | Unsupported
  -- ^ If the version that the implementation supersedes can never be adopted.
  deriving (Eq, Show, Generic)

-- | Check 'State' invariants. Note that invariants 0 and 1 are
-- satisfied by construction.
--
checkInvariants
  :: Implementation sip impl
  => State sip impl
  -> State sip impl
checkInvariants st = assert checks st
  where
    checks = do
      activationQueue st `doesNotContainKey`      currentProtocolVersion st
      activationQueue st `doesNotContainMaybeKey` endorsedProtocolVersion st

      currentProtocolVersion st <?!  endorsedProtocolVersion st
      currentProtocolVersion st ==?! endorsedSupersedesVersion st

      ((currentProtocolVersion st <=) . supersedesVersion)
        `holdsForAllElementsIn` activationQueue st
      ((endorsedProtocolVersion st ?<) . version)
        `holdsForAllElementsIn` activationQueue st

      -- If the last applied slot is greater than the end of the safety lag the
      -- candidate is either expired or adopted.
      --
      -- Versions are adopted in the first slot of a new epoch.
      lastAppliedSlot st ?<=?! candidateEndOfSafetyLag st

-- | Proposal (if any) in the endorsement period.
data MaybeAnEndorsedProposal sip impl
  = NoProposal
  | Candidate
    { cProtocol       :: !(Protocol impl)
    , cEndorsements   :: !(Endorsements impl)
    , cEndOfSafetyLag :: !SlotNo
      -- ^ Slot at which the safety lag expires. This __should be__ the
      -- __first__ slot of some epoch.
    }
  | Scheduled { sProtocol :: !(Protocol impl) }
    -- ^ Once the candidate receives enough endorsements it becomes scheduled.
  deriving (Generic)

deriving instance Implementation sip impl => Show (MaybeAnEndorsedProposal sip impl)

-- | The reason why we have two sets of endorsements is that endorsements that
-- come after the cut-off point should not be considered when tallying, which
-- will take place __after__ the cut-off point.
data Endorsements impl =
  Endorsements
  { thisEpochEndorsements :: !(Set (EndorserId (Protocol impl)))
  , nextEpochEndorsements :: !(Set (EndorserId (Protocol impl)))
    -- ^ Endorsements that come past the cut-off point are put in this set.
  } deriving (Generic)

deriving instance Activable (Protocol impl) => Show (Endorsements impl)

noEndorsements :: Activable (Protocol impl) => Endorsements impl
noEndorsements =
  Endorsements
  { thisEpochEndorsements = mempty
  , nextEpochEndorsements = mempty
  }

endorseInThisEpoch
  :: Activable (Protocol impl)
  => Endorsements impl
  -> EndorserId (Protocol impl)
  -> Endorsements impl
endorseInThisEpoch endorsements endorserId =
  endorsements
  { thisEpochEndorsements =
      Set.insert endorserId (thisEpochEndorsements endorsements)
  }

changeEpoch
  :: Activable (Protocol impl)
  => Endorsements impl -> Endorsements impl
changeEpoch endorsements =
  endorsements
  { thisEpochEndorsements = thisEpochEndorsements endorsements
                            `Set.union`
                            nextEpochEndorsements endorsements
  , nextEpochEndorsements = mempty
  }


endorseInNextEpoch
  :: Activable (Protocol impl)
  => Endorsements impl
  -> EndorserId (Protocol impl)
  -> Endorsements impl
endorseInNextEpoch endorsements endorserId =
  endorsements { nextEpochEndorsements =
                   Set.insert endorserId (nextEpochEndorsements endorsements)
               }

totalEndorsements
  :: ( HasStakeDistribution env (EndorserId (Protocol impl))
     , Implementation sip impl
     )
  => env
  -> Endorsements impl
  -> Stake
totalEndorsements env endorsements =
  stakeOfKeys' (thisEpochEndorsements endorsements)
               (stakeDistribution env)

--------------------------------------------------------------------------------
-- Interface functions
--------------------------------------------------------------------------------

initialState
  :: Implementation sip impl
  => Protocol impl
  -- ^ Initial protocol. This determines the current version.
  -> State sip impl
initialState initialProtocol
  = checkInvariants
  $ State
    { activationQueue    = mempty
    , endorsedProposal   = NoProposal
    , currentProtocol    = initialProtocol
    , applicationUpdates = mempty
    , lastAppliedSlot    = Nothing
    , discarded          = mempty
    }

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
   . ( TracksSlotTime env
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
  & tickLastAppliedSlot
  & checkInvariants

  where
    checkPreCondition =
      assert $ (currentSlot env - 1) ==?! lastAppliedSlot st

    tickLastAppliedSlot st' = st' { lastAppliedSlot = Just (currentSlot env) }

    tryTransferEndorsements st' =
      case endorsedProposal st' of
        Candidate { cProtocol, cEndorsements, cEndOfSafetyLag }
          | currentSlot env == epochFirstSlot env
            -> st { endorsedProposal =
                      Candidate
                      { cProtocol = cProtocol
                      , cEndorsements   = changeEpoch cEndorsements
                      , cEndOfSafetyLag = cEndOfSafetyLag
                      }
                  }
        _ -> st'

    -- | Activate an eventual candidate protocol version
    --
    -- TODO: discuss this with Edsko.
    tryActivate st' =
      case endorsedProposal st' of
        Scheduled { sProtocol }
          | currentSlot env == epochFirstSlot env ->
            deleteProposalsThatCannotFollow
            $ st' { endorsedProposal = NoProposal
                  , currentProtocol   = sProtocol
                  }
        _                                         -> st'

    tryTally st' =
      case endorsedProposal st' of
        Candidate { cProtocol, cEndorsements, cEndOfSafetyLag }
          | atTallyPoint && candidateHasEnough cEndorsements cEndOfSafetyLag
            -> scheduleCandidate cProtocol
          | (atTallyPoint || pastTallyPoint)
            && noNextEndorsementPeriod cEndOfSafetyLag env
            ->
             -- If the a proposal became candidate after the cutoff period, but
             -- has no next endorsement period it should be expired, since no
             -- endorsements can be considered in the epoch, and there is no
             -- next endorsement period.
             expireCandidate cProtocol
        _   -> st'
      where
        atTallyPoint = currentSlot env == tallySlot env

        pastTallyPoint = tallySlot env < currentSlot env

        candidateHasEnough endorsements endOfSafetyLag =
          adjustedAdoptionThreshold <= totalEndorsements env endorsements
          where
            adjustedAdoptionThreshold
              | noNextEndorsementPeriod endOfSafetyLag env
              = round $ 0.51 * (fromIntegral endorsersTotalStake :: Double)
              | otherwise
              = stakeThreshold (adversarialStakeRatio env)
                               endorsersTotalStake

              where
                endorsersTotalStake =
                  totalStake @env @(EndorserId (Protocol impl)) env

        expireCandidate cProtocol                =
          st' { endorsedProposal = NoProposal
              , discarded        =
                  Map.insert cProtocol
                             Expired
                             (discarded st')
              }
        scheduleCandidate implAndHash                  =
          st' { endorsedProposal = Scheduled { sProtocol = implAndHash } }

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
  :: ( TracksSlotTime env
     , Implementation sip impl
     )
  => env
  -> Approval.State impl
  -> State sip impl
  -> (Approval.State impl, State sip impl)
transferApprovals env approvalSt st = (approvalSt', st')
  where
    (approved, approvalSt') = Approval.removeApproved approvalSt
    -- Cancellations are applied at the end, in case a proposal gets approved
    -- and canceled in the same slot. This shouldn't happen in practice, since
    -- the experts should decide on one or the other instead of a approving the
    -- update and its cancellation at the same time.
    st'                     = foldl dispatch st approved
                            & checkInvariants

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
endorse
  :: ( TracksSlotTime env
     , Implementation sip impl
     )
  => env
  -> Endorsement sip impl
  -> State sip impl
  -> Either (Error sip impl) (State sip impl)
endorse env Endorsement {endorserId, endorsedVersion} st =
  case endorsedProposal st of
    Candidate { cProtocol }
      | version cProtocol == endorsedVersion
        -> Right $ endorseUnchecked endorserId env st
    _   -> Left  $ ProtocolVersionIsNotACandidate
                   { endorsementErrorGivenProtocolVersion = endorsedVersion
                   , endorsementErrorEndorsedProposal     = endorsedProposal st
                   }

endorseUnchecked
  :: ( TracksSlotTime env
     , Implementation sip impl
     )
  => EndorserId (Protocol impl)
  -> env
  -> State sip impl
  -> State sip impl
endorseUnchecked endorserId env st
  = checkInvariants
  $ case endorsedProposal st of
      Candidate { cProtocol, cEndorsements, cEndOfSafetyLag }
        | beforeCutoffPeriod env
          -> st { endorsedProposal =
                    Candidate
                    { cProtocol = cProtocol
                    , cEndorsements   = cEndorsements `endorseInThisEpoch` endorserId
                    , cEndOfSafetyLag = cEndOfSafetyLag
                    }
                }
        | otherwise
          -> st { endorsedProposal =
                    Candidate
                    { cProtocol = cProtocol
                    , cEndorsements   = cEndorsements `endorseInNextEpoch` endorserId
                    , cEndOfSafetyLag = cEndOfSafetyLag
                    }
                }
      _ -> error "No candidate to endorse"

-- | Is the current slot before the cut-off slot?
beforeCutoffPeriod :: TracksSlotTime env => env -> Bool
beforeCutoffPeriod env =
  currentSlot env < cutOffSlot env

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
  | cannotFollowCurrentVersion || cannotFollowScheduledProposal = st
  | otherwise
  = st { activationQueue = Map.insert protocolVersion protocol (activationQueue st)
       }
  & resolveConflictWithCandidate
  & tryFindNewCandidate env
  where
    protocolVersion                  = version protocol
    protocolSupersedesVersion        =
      supersedesVersion protocol
    cannotFollowCurrentVersion       =
      protocolSupersedesVersion < currentProtocolVersion st
    cannotFollowScheduledProposal    =
      case endorsedProposal st of
        Scheduled { sProtocol } ->
          protocolSupersedesVersion < version sProtocol
        _           ->
          False
    resolveConflictWithCandidate st' =
      case endorsedProposal st' of
        Candidate { cProtocol }
          | protocolVersion == version cProtocol
            -> st' { endorsedProposal = NoProposal
                   , discarded        =
                       Map.insert cProtocol Obsoleted (discarded st')
                   }
          | protocolVersion < version cProtocol
            -> st' { activationQueue  =
                       Map.insert (version cProtocol)
                                  cProtocol
                                  (activationQueue st')
                   , endorsedProposal = NoProposal
                   }
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
  case (endorsedProposal st, maybeNextProposal) of
    (NoProposal, Just nextProtocol) -> makeCandidate nextProtocol
    (_         , _                ) -> st
  where
    maybeNextProposal = nextCandidate st
    makeCandidate nextProtocol
      = st { activationQueue  =
               Map.delete (version nextProtocol) (activationQueue st)
           , endorsedProposal =
               Candidate
               { cProtocol       = nextProtocol
               , cEndorsements   = noEndorsements
               , cEndOfSafetyLag =
                 -- TODO: we need to make the safety lag configurable in
                 -- 'ImplementationData'.
                   nextEpochFirstSlot env + slotsPerEpoch env
               }
           }

-- | Cancel an update proposal.
--
-- Only scheduled proposals cannot be canceled.
cancel
  :: (Implementation sip impl)
  => State sip impl
  -> ProtocolId impl
  -> State sip impl
cancel st toCancel
  = discardProposalsThatDoNotSatisfy ((/= toCancel) . _id) Canceled st
  & cancelEventualCandidate
  where
    cancelEventualCandidate st' =
      case endorsedProposal st' of
        Candidate { cProtocol }
          | _id cProtocol == toCancel
            -> st' { endorsedProposal = NoProposal
                   , discarded        = Map.insert cProtocol
                                                   Canceled
                                                   (discarded st')
                   }

        _   -> st'

updateApplication
  :: (Implementation sip impl)
  => State sip impl
  -> Application impl
  -> State sip impl
updateApplication st implementation
  = checkPreconditions
  $ st { applicationUpdates = implementation : (applicationUpdates st) }
  where
    checkPreconditions = assert $ do
      fmap _id (applicationUpdates st) `doesNotContain` _id implementation

discardProposalsThatDoNotSatisfy
  :: (Implementation sip impl)
  => (Protocol impl -> Bool)
  -> Reason
  -> State sip impl
  -> State sip impl
discardProposalsThatDoNotSatisfy predicate reason st =
  st { activationQueue = keep
     , discarded       = foldl' (\m implAndHash -> Map.insert implAndHash reason m)
                                (discarded st)
                                (Map.elems discard)
     }
  where
    (keep, discard) = Map.partition predicate (activationQueue st)

--------------------------------------------------------------------------------
-- Queue related operations
--------------------------------------------------------------------------------

-- | Find the proposal with the lowest version that supersedes the given current
-- version.
nextCandidate
  :: (Implementation sip impl)
  => State sip impl
  -> Maybe (Protocol impl)
nextCandidate st =
  case Map.lookupMin (activationQueue st) of
    Just (_, minProtocol)
      | currentProtocolVersion st == supersedesVersion minProtocol
        -> Just minProtocol
    _ -> Nothing

-- | Delete those proposals that cannot follow the current version.
deleteProposalsThatCannotFollow
  :: (Implementation sip impl)
  => State sip impl
  -> State sip impl
deleteProposalsThatCannotFollow st =
  discardProposalsThatDoNotSatisfy canFollow Unsupported st
  where
    canFollow = (currentProtocolVersion st <=)
              . supersedesVersion

--------------------------------------------------------------------------------
-- Update state query operations
--------------------------------------------------------------------------------

class Implementation sip impl
      => HasActivationState st sip impl | st -> sip, st -> impl  where
  getActivationState :: st -> State sip impl

instance Implementation sip impl
         => HasActivationState (State sip impl) sip impl where
  getActivationState = id

getCurrentProtocol :: HasActivationState st sip impl => st -> (Protocol impl)
getCurrentProtocol = currentProtocol . getActivationState

getCurrentProtocolVersion
  :: HasActivationState st sip impl => st -> Version (Protocol impl)
getCurrentProtocolVersion = version . getCurrentProtocol

-- | Was the given protocol update implementation activated?
--
-- In this context, the fact that an update is "activated" means that it is the
-- current version.
--
-- For software and cancellation updates this function will always return
-- 'False'.
isTheCurrentVersion
  :: (HasActivationState st sip impl)
  => ProtocolId impl -> st -> Bool
isTheCurrentVersion h = (== h) . _id . currentProtocol . getActivationState

isScheduled
  :: (HasActivationState st sip impl)
  => ProtocolId impl -> st -> Bool
isScheduled h =
  fromMaybe False . fmap (== h) . scheduledProtocolId . getActivationState

isBeingEndorsed
  :: (HasActivationState st sip impl)
  => ProtocolId impl -> st -> Bool
isBeingEndorsed h =
  fromMaybe False . fmap (== h) . endorsedProtocolId . getActivationState

isQueued
  :: (HasActivationState st sip impl)
  => ProtocolId impl -> st -> Bool
isQueued h st = h `elem` fmap _id queuedImplementations
  where
    queuedImplementations = Map.elems $ activationQueue $ getActivationState st

isDiscardedDueToBeing
  :: (HasActivationState st sip impl)
  => ProtocolId impl -> Reason -> st -> Bool
isDiscardedDueToBeing h reason =
  (h `elem`) . discardedHashesDueTo reason

discardedHashesDueTo
  :: HasActivationState st sip impl
  => Reason -> st -> [ProtocolId impl]
discardedHashesDueTo reason
  = fmap _id
  . Map.keys
  . Map.filter (== reason)
  . discarded
  . getActivationState

endorsedProtocol
  :: HasActivationState st sip impl => st -> Maybe (Protocol impl)
endorsedProtocol st =
  case endorsedProposal (getActivationState st) of
    Candidate { cProtocol } -> Just cProtocol
    Scheduled { sProtocol } -> Just sProtocol
    _                       -> Nothing

candidateEndOfSafetyLag
  :: HasActivationState st sip impl => st -> Maybe SlotNo
candidateEndOfSafetyLag st =
  case endorsedProposal (getActivationState st) of
    Candidate { cEndOfSafetyLag } -> Just cEndOfSafetyLag
    _                             -> Nothing

scheduledProtocol
  :: HasActivationState st sip impl => st -> Maybe (Protocol impl)
scheduledProtocol st =
  case endorsedProposal (getActivationState st) of
    Scheduled { sProtocol } -> Just sProtocol
    _                       -> Nothing

endorsedProtocolVersion
  :: (HasActivationState st sip impl) => st -> Maybe (Version (Protocol impl))
endorsedProtocolVersion
  = fmap version . endorsedProtocol

endorsedProtocolId
  :: HasActivationState st sip impl => st -> Maybe (ProtocolId impl)
endorsedProtocolId =
    fmap _id . endorsedProtocol

endorsedSupersedesVersion
  :: (HasActivationState st sip impl) => st -> Maybe (Version (Protocol impl))
endorsedSupersedesVersion =
  fmap supersedesVersion . endorsedProtocol

currentProtocolVersion
  :: (HasActivationState st sip impl) => st -> Version (Protocol impl)
currentProtocolVersion =
  version  . currentProtocol . getActivationState

scheduledProtocolId
  :: HasActivationState st sip impl => st -> Maybe (ProtocolId impl)
scheduledProtocolId
  = fmap _id . scheduledProtocol . getActivationState

class HasActivationError err sip impl | err -> sip, err -> impl where
  getActivationError :: err -> Maybe (Error sip impl)

instance HasActivationError (Error sip impl) sip impl where
  getActivationError = Just . id

endorsedVersionError
  :: HasActivationError err sip impl => err -> Maybe (Version (Protocol impl))
endorsedVersionError
  = fmap endorsementErrorGivenProtocolVersion . getActivationError
