{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Update.Activation
  ( State
    -- * Interface functions
  , initialState
  , tick
  , transferApprovals
  , endorse
    -- ** Endorsement construction
  , Endorsement (Endorsement)
    -- ** Error reporting
  , Error
  , HasActivationError (getActivationError)
  , endorsementErrorGivenProtocolVersion
  , endorsementErrorEndorsedProposal
  , endorsedVersionError
    -- * Update state query operations
  , HasActivationState (getActivationState)
  , getCurrentVersion
  , currentVersion
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

import           Ledger.Core (Slot, addSlot, minusSlot)

import           Cardano.Ledger.Spec.Classes.HasAdversarialStakeRatio
                     (HasAdversarialStakeRatio, adversarialStakeRatio)
import           Cardano.Ledger.Spec.Classes.Hashable (Hash, Hashable)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (VKey)
import           Cardano.Ledger.Spec.Classes.HasStakeDistribution
                     (HasStakeDistribution, StakePools (StakePools),
                     stakeDistribution, totalStake)
import           Cardano.Ledger.Spec.Classes.TracksSlotTime (TracksSlotTime,
                     currentSlot, epochFirstSlot, nextEpochFirstSlot,
                     slotsPerEpoch, stableAfter)
import           Cardano.Ledger.Spec.State.StakeDistribution (stakeOfKeys')
import           Cardano.Ledger.Spec.STS.Update.Approval.Data
                     (ImplementationAndHash (ImplementationAndHash),
                     ImplementationData, ProtocolVersion,
                     UpdateType (Application, Cancellation, Protocol),
                     implData, implHash, implType, implementationSupersedes,
                     implementationVersion, isApplicationUpdate, supersedes,
                     toCancel, version)
import           Cardano.Ledger.Spec.STS.Update.Data (Stake)
import           Cardano.Ledger.Spec.STS.Update.Definitions (stakeThreshold)

import qualified Cardano.Ledger.Update.Approval as Approval

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
data State p =
  State
  { endorsedProposal   :: !(MaybeAnEndorsedProposal p)
  , currentVersion     :: !(ImplementationAndHash p)
  , activationQueue    :: !(Map ProtocolVersion (ImplementationAndHash p))
    -- ^ The activation queue maintains a priority queue of protocol versions.
    -- The lower the version the higher the priority. Duplicated versions are
    -- not allowed. Inserting a duplicated version replaces the (implementation
    -- and hash of the) older version.
  , applicationUpdates :: ![ImplementationData p]
    -- ^ Approved application updates.
    --
    -- TODO: this is a list for now. If needed this can be turned into a set, or
    -- a map indexed by application names.
    --
    -- TODO: the application updates are not part of the 'proposalsMap', but we
    -- can add them if needed.
  , lastAppliedSlot    :: !(Maybe Slot)
    -- ^ We require that the 'tick' function is applied without skipping any
    -- slot. Therefore we record the last applied slot to check this
    -- pre-condition in 'tick'.
    --
    -- TODO: we might want to move last applied slot and invariant checking to
    -- the update interface state. Otherwise we'll have to replicate this in all
    -- the three phases (in which we can't miss a slot tick).

  , discarded          :: !(Map (ImplementationAndHash p) Reason)
    -- TODO: the stakepoolsDistribution snapshot should probably go here.
  } deriving (Show, Generic)

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
  :: Hashable p
  => State p
  -> State p
checkInvariants st = assert checks st
  where
    checks = do
      activationQueue st `doesNotContainKey`      currentProtocolVersion st
      activationQueue st `doesNotContainMaybeKey` endorsedProtocolVersion st

      currentProtocolVersion st <?!  endorsedProtocolVersion st
      currentProtocolVersion st ==?! endorsedSupersedes st

      ((currentProtocolVersion st <=) . supersedes) `holdsForAllElementsIn` activationQueue st
      ((endorsedProtocolVersion st ?<) . version)   `holdsForAllElementsIn` activationQueue st

      -- If the last applied slot is greater than the end of the safety lag the
      -- candidate is either expired or adopted.
      --
      -- Versions are adopted in the first slot of a new epoch.
      lastAppliedSlot st ?<=?! candidateEndOfSafetyLag st

-- | Proposal (if any) in the endorsement period.
data MaybeAnEndorsedProposal p
  = NoProposal
  | Candidate
    { cImplementation :: !(ImplementationAndHash p)
    , cEndorsements   :: !(Endorsements p)
    , cEndOfSafetyLag :: !Slot
      -- ^ Slot at which the safety lag expires. This __should be__ the
      -- __first__ slot of some epoch.
    }
  | Scheduled { sImplementation :: !(ImplementationAndHash p) }
    -- ^ Once the candidate receives enough endorsements it becomes scheduled.
  deriving (Eq, Show, Generic)

-- | The reason why we have two sets of endorsements is that endorsements that
-- come after the cut-off point should not be considered when tallying, which
-- will take place __after__ the cut-off point.
data Endorsements p =
  Endorsements
  { thisEpochEndorsements :: !(Set (Hash p (VKey p)))
  , nextEpochEndorsements :: !(Set (Hash p (VKey p)))
    -- ^ Endorsements that come past the cut-off point are put in this set.
  } deriving (Generic)

deriving instance Hashable p => Show (Endorsements p)
deriving instance Hashable p => Eq (Endorsements p)

noEndorsements :: Hashable p => Endorsements p
noEndorsements =
  Endorsements
  { thisEpochEndorsements = mempty
  , nextEpochEndorsements = mempty
  }

endorseInThisEpoch
  :: Hashable p
  => Endorsements p
  -> Hash p (VKey p)
  -> Endorsements p
endorseInThisEpoch endorsements keyHash =
  endorsements
  { thisEpochEndorsements =
      Set.insert keyHash (thisEpochEndorsements endorsements)
  }

changeEpoch
  :: Hashable p
  => Endorsements p -> Endorsements p
changeEpoch endorsements =
  endorsements
  { thisEpochEndorsements = thisEpochEndorsements endorsements
                            `Set.union`
                            nextEpochEndorsements endorsements
  , nextEpochEndorsements = mempty
  }


endorseInNextEpoch
  :: Hashable p
  => Endorsements p
  -> Hash p (VKey p)
  -> Endorsements p
endorseInNextEpoch endorsements keyHash =
  endorsements { nextEpochEndorsements =
                   Set.insert keyHash (nextEpochEndorsements endorsements)
               }

-- | We use total stake for counting endorsements.
type EndorsementCount = Stake

totalEndorsements
  :: ( Hashable p
     , HasStakeDistribution StakePools e p
     )
  => e
  -> Endorsements p
  -> EndorsementCount
totalEndorsements env endorsements =
  stakeOfKeys' (thisEpochEndorsements endorsements)
               (stakeDistribution StakePools env)


--------------------------------------------------------------------------------
-- Interface functions
--------------------------------------------------------------------------------

initialState
  :: Hashable p
  => Hash p (ImplementationData p)
  -> ImplementationData p
  -- ^ Initial implementation. This determines the current version.
  -> State p
initialState initialHash initialImplData
  = checkInvariants
  $ State
    { activationQueue    = mempty
    , endorsedProposal   = NoProposal
    , currentVersion     = initialImplAndHash
    , applicationUpdates = mempty
    , lastAppliedSlot    = Nothing
    , discarded          = mempty
    }
  where
    initialImplAndHash =
      ImplementationAndHash
      { implHash = initialHash
      , implData = initialImplData
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
  :: ( Hashable p
     , TracksSlotTime e
     , HasStakeDistribution StakePools e p
     , HasAdversarialStakeRatio e
     )
  => e
  -> State p
  -> State p
tick env st
  = checkPreCondition st
  & tryTransferEndorsements
  & tryActivate
  & tryTally
  & tryFindNewCandidate env -- We do this at the end, since activation or
                            -- expiration cause the next proposal to be put in the
                            -- queue.
--  & tryToTakeStakeDistributionSnapshot
  & tickLastAppliedSlot
  & checkInvariants

  where
    checkPreCondition =
      assert $ currentSlot env `minusSlot` 1 ==?! lastAppliedSlot st

    tickLastAppliedSlot st' = st' { lastAppliedSlot = Just (currentSlot env) }

    tryTransferEndorsements st' =
      case endorsedProposal st' of
        Candidate { cImplementation, cEndorsements, cEndOfSafetyLag }
          | currentSlot env == epochFirstSlot env
            -> st { endorsedProposal =
                      Candidate
                      { cImplementation = cImplementation
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
        Scheduled { sImplementation }
          | currentSlot env == epochFirstSlot env ->
            deleteProposalsThatCannotFollow
            $ st' { endorsedProposal = NoProposal
                  , currentVersion   = sImplementation
                  }
        _                                         -> st'

    tryTally st' =
      case endorsedProposal st' of
        Candidate { cImplementation, cEndorsements, cEndOfSafetyLag }
          | atTallyPoint && candidateHasEnough cEndorsements cEndOfSafetyLag
            -> scheduleCandidate cImplementation
          | (atTallyPoint || pastTallyPoint)
            && noNextEndorsementPeriod cEndOfSafetyLag env
            ->
             -- If the a proposal became candidate after the cutoff period, but
             -- has no next endorsement period it should be expired, since no
             -- endorsements can be considered in the epoch, and there is no
             -- next endorsement period.
             expireCandidate cImplementation
        _   -> st'
      where
        atTallyPoint = currentSlot env == tallySlot env

        pastTallyPoint = tallySlot env < currentSlot env

        candidateHasEnough endorsements endOfSafetyLag =
          adjustedAdoptionThreshold <= totalEndorsements env endorsements
          where
            adjustedAdoptionThreshold
              | noNextEndorsementPeriod endOfSafetyLag env
              = round $ 0.51 * (fromIntegral $ totalStake StakePools env :: Double)
              | otherwise
              = stakeThreshold (adversarialStakeRatio env)
                               (totalStake StakePools env)
        expireCandidate cImplementation                =
          st' { endorsedProposal = NoProposal
              , discarded        =
                  Map.insert cImplementation
                             Expired
                             (discarded st')
              }
        scheduleCandidate implAndHash                  =
          st' { endorsedProposal = Scheduled { sImplementation = implAndHash } }

noNextEndorsementPeriod
  :: TracksSlotTime e
  => Slot
  -> e
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
  :: ( Hashable p
     , TracksSlotTime e
     )
  => e
  -> Approval.State p
  -> State p
  -> (Approval.State p, State p)
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
    dispatch st'' (iHash, i) =
      case implType i of
        Cancellation { toCancel } -> foldl' cancel st'' toCancel
        Protocol {}               -> addProtocolUpdateProposal env st'' (iHash, i)
        Application {}            -> updateApplication st'' i


-- TODO: if the endorsements come in transactions we need to prevent endorsement
-- replays, and we need the endorsement to be signed.
--
-- TODO: check: if endorsements come into blocks, since only block issuers can
-- endorse, then we shouldn't worry about replay attacks and forgery.
data Endorsement p =
  Endorsement
  { endorsingKeyHash :: !(Hash p (VKey p))
  , endorsedVersion  :: !ProtocolVersion
  } deriving (Generic)

deriving instance Hashable p => Show (Endorsement p)

data Error p =
  ProtocolVersionIsNotACandidate
  { endorsementErrorGivenProtocolVersion :: !ProtocolVersion
  , endorsementErrorEndorsedProposal     :: !(MaybeAnEndorsedProposal p)
  } deriving (Eq, Show, Generic)

-- | Register an endorsement.
endorse
  :: ( Hashable p
     , TracksSlotTime env
     )
  => env
  -> Endorsement p
  -> State p
  -> Either (Error p) (State p)
endorse env Endorsement {endorsingKeyHash, endorsedVersion} st =
  case endorsedProposal st of
    Candidate { cImplementation }
      | version cImplementation == endorsedVersion
        -> Right $ endorseUnchecked endorsingKeyHash env st
    _   -> Left  $ ProtocolVersionIsNotACandidate
                   { endorsementErrorGivenProtocolVersion = endorsedVersion
                   , endorsementErrorEndorsedProposal     = endorsedProposal st
                   }

endorseUnchecked
  :: ( Hashable p
     , TracksSlotTime e
     )
  => Hash p (VKey p)
  -> e
  -> State p
  -> State p
endorseUnchecked keyHash env st
  = checkInvariants
  $ case endorsedProposal st of
      Candidate { cImplementation, cEndorsements, cEndOfSafetyLag }
        | beforeCutoffPeriod env
          -> st { endorsedProposal =
                    Candidate
                    { cImplementation = cImplementation
                    , cEndorsements   = cEndorsements `endorseInThisEpoch` keyHash
                    , cEndOfSafetyLag = cEndOfSafetyLag
                    }
                }
        | otherwise
          -> st { endorsedProposal =
                    Candidate
                    { cImplementation = cImplementation
                    , cEndorsements   = cEndorsements `endorseInNextEpoch` keyHash
                    , cEndOfSafetyLag = cEndOfSafetyLag
                    }
                }
      _ -> error "No candidate to endorse"

-- | Is the current slot before the cut-off slot?
beforeCutoffPeriod :: TracksSlotTime e => e -> Bool
beforeCutoffPeriod env =
  currentSlot env < cutOffSlot env

cutOffSlot :: TracksSlotTime e => e -> Slot
cutOffSlot env = nextEpochFirstSlot env `minusSlot` (2 * (stableAfter env))

tallySlot :: TracksSlotTime e => e -> Slot
tallySlot env = nextEpochFirstSlot env `minusSlot` (stableAfter env)

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
  :: ( Hashable p
     , TracksSlotTime e
     )
  => e
  -> State p
  -> (Hash p (ImplementationData p), ImplementationData p)
  -> State p
addProtocolUpdateProposal env st (newImplHash, newImpl)
  | cannotFollowCurrentVersion || cannotFollowScheduledProposal = st
  | otherwise
  = st { activationQueue = Map.insert newImplVersion newImplAndHash (activationQueue st)
       }
  & resolveConflictWithCandidate
  & tryFindNewCandidate env
  where
    newImplVersion                     =
      implementationVersion newImpl
    newImplAndHash                     =
      ImplementationAndHash
      { implHash = newImplHash
      , implData = newImpl
      }
    newImplSupersedes                  =
      implementationSupersedes newImpl
    cannotFollowCurrentVersion         =
      newImplSupersedes < currentProtocolVersion st
    cannotFollowScheduledProposal      =
      case endorsedProposal st of
        Scheduled { sImplementation } ->
          newImplSupersedes < version sImplementation
        _           ->
          False
    resolveConflictWithCandidate st' =
      case endorsedProposal st' of
        Candidate { cImplementation }
          | newImplVersion == version cImplementation
            -> st' { endorsedProposal = NoProposal
                   , discarded        =
                       Map.insert cImplementation Obsoleted (discarded st')
                   }
          | newImplVersion < version cImplementation
            -> st' { activationQueue  =
                       Map.insert (version cImplementation)
                                  cImplementation
                                  (activationQueue st')
                   , endorsedProposal = NoProposal
                   }
        _   -> st'


-- | If there is candidate or scheduled proposal, determine whether there is a
-- proposal in the queue that should become the next candidate.
tryFindNewCandidate
  :: ( Hashable p
     , TracksSlotTime e
     )
  => e
  -> State p
  -> State p
tryFindNewCandidate env st =
  case (endorsedProposal st, maybeNextProposal) of
    (NoProposal, Just (nextVersion, nextImplAndHash)) -> makeCandidate (nextVersion, nextImplAndHash)
    (_         , _                                  ) -> st
  where
    maybeNextProposal = nextCandidate st
    makeCandidate (nextVersion, nextImplAndHash)
      = st { activationQueue  =
               Map.delete nextVersion (activationQueue st)
           , endorsedProposal =
               Candidate
               { cImplementation = nextImplAndHash
               , cEndorsements   = noEndorsements
               , cEndOfSafetyLag =
                 -- TODO: we need to make the safety lag configurable in
                 -- 'ImplementationData'.
                   nextEpochFirstSlot env `addSlot` slotsPerEpoch env
               }
           }

-- | Cancel an update proposal.
--
-- Only scheduled proposals cannot be canceled.
cancel
  :: Hashable p
  => State p
  -> Hash p (ImplementationData p)
  -> State p
cancel st toCancel
  = discardProposalsThatDoNotSatisfy ((/= toCancel) . implHash) Canceled st
  & cancelEventualCandidate
  where
    cancelEventualCandidate st' =
      case endorsedProposal st' of
        Candidate { cImplementation }
          | implHash cImplementation == toCancel
            -> st' { endorsedProposal = NoProposal
                   , discarded        = Map.insert cImplementation
                                                   Canceled
                                                   (discarded st')
                   }

        _   -> st'

updateApplication
  :: Hashable p
  => State p
  -> ImplementationData p
  -> State p
updateApplication st implementation
  = checkPreconditions
  $ st { applicationUpdates = implementation : (applicationUpdates st) }
  where
    checkPreconditions = assert $ do
      isApplicationUpdate implementation
        `orElseShow` ("Expecting an application, instead we got: "
                      <> cShow implementation
                     )
      applicationUpdates st `doesNotContain` implementation

discardProposalsThatDoNotSatisfy
  :: Hashable p
  => (ImplementationAndHash p -> Bool)
  -> Reason
  -> State p
  -> State p
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
  :: Hashable p => State p
  -> Maybe (ProtocolVersion, ImplementationAndHash p)
nextCandidate st =
  case Map.lookupMin (activationQueue st) of
    Just (minVersion, minImplAndHash)
      | currentProtocolVersion st == supersedes minImplAndHash
        -> Just (minVersion, minImplAndHash)
    _ -> Nothing

-- | Delete those proposals that cannot follow the current version.
deleteProposalsThatCannotFollow
  :: Hashable p
  => State p
  -> State p
deleteProposalsThatCannotFollow st =
  discardProposalsThatDoNotSatisfy canFollow Unsupported st
  where
    canFollow = (currentProtocolVersion st <=)
              . implementationSupersedes
              . implData

--------------------------------------------------------------------------------
-- Update state query operations
--------------------------------------------------------------------------------

class HasActivationState st p | st -> p where
  getActivationState :: st -> State p

instance HasActivationState (State p) p where
  getActivationState = id

getCurrentVersion :: HasActivationState st p => st -> ImplementationAndHash p
getCurrentVersion = currentVersion . getActivationState

-- | Was the given protocol update implementation activated?
--
-- In this context, the fact that an update is "activated" means that it is the
-- current version.
--
-- For software and cancellation updates this function will always return
-- 'False'.
isTheCurrentVersion
  :: (Hashable p, HasActivationState st p)
  => Hash p (ImplementationData p) -> st -> Bool
isTheCurrentVersion h = (== h) . implHash . currentVersion . getActivationState

isScheduled
  :: (Hashable p, HasActivationState st p)
  => Hash p (ImplementationData p) -> st -> Bool
isScheduled h =
  fromMaybe False . fmap (== h) . scheduledImplementationHash . getActivationState

isBeingEndorsed
  :: (Hashable p, HasActivationState st p)
  => Hash p (ImplementationData p) -> st -> Bool
isBeingEndorsed h =
  fromMaybe False . fmap (== h) . endorsedImplementationHash . getActivationState

isQueued
  :: (Hashable p, HasActivationState st p)
  => Hash p (ImplementationData p) -> st -> Bool
isQueued h st = h `elem` fmap implHash queuedImplementations
  where
    queuedImplementations = Map.elems $ activationQueue $ getActivationState st

isDiscardedDueToBeing
  :: (Hashable p, HasActivationState st p)
  => Hash p (ImplementationData p) -> Reason -> st -> Bool
isDiscardedDueToBeing h reason =
  (h `elem`) . discardedHashesDueTo reason

discardedHashesDueTo
  :: HasActivationState st p
  => Reason -> st -> [Hash p (ImplementationData p)]
discardedHashesDueTo reason
  = fmap implHash
  . Map.keys
  . Map.filter (== reason)
  . discarded
  . getActivationState

endorsedImplementation
  :: HasActivationState st p => st -> Maybe (ImplementationAndHash p)
endorsedImplementation st =
  case endorsedProposal (getActivationState st) of
    Candidate { cImplementation } -> Just cImplementation
    Scheduled { sImplementation } -> Just sImplementation
    _                             -> Nothing

candidateEndOfSafetyLag
  :: HasActivationState st p => st -> Maybe Slot
candidateEndOfSafetyLag st =
  case endorsedProposal (getActivationState st) of
    Candidate { cEndOfSafetyLag } -> Just cEndOfSafetyLag
    _                             -> Nothing

scheduledImplementation
  :: HasActivationState st p => st -> Maybe (ImplementationAndHash p)
scheduledImplementation st =
  case endorsedProposal (getActivationState st) of
    Scheduled { sImplementation } -> Just sImplementation
    _                             -> Nothing

endorsedProtocolVersion
  :: (Hashable p, HasActivationState st p) => st -> Maybe ProtocolVersion
endorsedProtocolVersion
  = fmap (implementationVersion . implData) . endorsedImplementation

endorsedImplementationHash
  :: HasActivationState st p => st -> Maybe (Hash p (ImplementationData p))
endorsedImplementationHash =
    fmap implHash . endorsedImplementation

endorsedSupersedes
  :: (Hashable p, HasActivationState st p) => st -> Maybe ProtocolVersion
endorsedSupersedes =
  fmap (implementationSupersedes . implData) . endorsedImplementation

currentProtocolVersion
  :: (Hashable p, HasActivationState st p) => st -> ProtocolVersion
currentProtocolVersion =
  implementationVersion . implData  . currentVersion . getActivationState

scheduledImplementationHash
  :: HasActivationState st p => st -> Maybe (Hash p (ImplementationData p))
scheduledImplementationHash
  = fmap implHash . scheduledImplementation . getActivationState


class HasActivationError err p | err -> p where
  getActivationError :: err -> Maybe (Error p)

instance HasActivationError (Error p) p where
  getActivationError = Just . id

endorsedVersionError
  :: HasActivationError err p => err  -> Maybe ProtocolVersion
endorsedVersionError
  = fmap endorsementErrorGivenProtocolVersion . getActivationError
