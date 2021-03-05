{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Activation state.
module Cardano.Ledger.Update.Activation.State
  ( -- * Data types
    State
  , Reason (Expired, Canceled, Unsupported)
  , MaybeAnEndorsedProposal (NoProposal, Candidate, Scheduled)
  , Endorsements
    -- * State operations
  , initialState
    -- ** Operation on the proposals queue
  , enqueue
  , deleteProposalsThatCannotFollow
  , obsoleteProposal
  , discardProposalsThatDoNotSatisfy
    -- ** Operations on candidates
  , endorsedProposal
  , cEndorsements
  , cEndOfSafetyLag
  , findANewCandidate
  , discardCandidate
  , reEnqueueCandidate
  , scheduleCandidate
  , activate
    -- ** Operations on applications
  , addApplication
    -- ** Operations on the last applied slot
  , lastAppliedSlot
  , tickLastAppliedSlot
    -- ** Operations on endorsements
  , endorseInThisEpoch
  , endorseInNextEpoch
  , transferEndorsements
  , thisEpochEndorsements
    -- ** State query operations
  , HasActivationState (getActivationState)
  , currentProtocolVersion
  , endorsedProtocol
  , endorsedProtocolVersion
  , endorsedSupersedesVersion
  , candidateProtocol
  , scheduledProtocol
  , scheduledProtocolVersion
  , getCurrentProtocol
  , getCurrentProtocolId
  , getCurrentProtocolVersion
  , currentProtocol
  , isQueued
  , isBeingEndorsed
  , isScheduled
  , isTheCurrentVersion
  , candidateEndOfSafetyLag
  , isDiscardedDueToBeing
  , queuedProtocols
  , candidateProtocols
  , endOfSafetyLag
  )
where

import           Control.DeepSeq (NFData)
import           Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           Data.List (find, foldl')
import           Data.Map.Strict (Map)
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.Set (Set)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR),
                     decodeInt, decodeListLen, decodeListLenOf, encodeInt,
                     encodeListLen)
import           Cardano.Slotting.Slot (SlotNo)

import           Cardano.Ledger.Update.Env.TracksSlotTime (TracksSlotTime,
                     nextEpochFirstSlot, slotsPerEpoch)
import           Cardano.Ledger.Update.Proposal

import           Cardano.Ledger.Assert


-- | Activation state.
--
-- INVARIANT:
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

deriving instance
  ( Implementation sip impl
  , Eq (Application impl)
  ) => Eq (State sip impl)

deriving instance
  ( NFData (Protocol impl)
  , NFData (Version (Protocol impl))
  , NFData (Application impl)
  , NFData (Id (Endorser (Protocol impl)))
  ) => NFData (State sip impl)

deriving instance
  ( NoThunks (Protocol impl)
  , NoThunks (Version (Protocol impl))
  , NoThunks (Application impl)
  , NoThunks (Id (Endorser (Protocol impl)))
  ) => NoThunks (State sip impl)

deriving instance
  ( ToJSONKey (Protocol impl)
  , ToJSON (Application impl)
  , ToJSONKey (Version (Protocol impl))
  , ToJSON (Protocol impl)
  , ToJSON (Id (Endorser (Protocol impl)))
  ) => ToJSON (State sip impl)

deriving instance
  ( FromJSON (Protocol impl)
  , FromJSON (Application impl)
  , FromJSONKey (Version (Protocol impl))
  , FromJSONKey (Protocol impl)
  , Implementation sip impl
  , Activable impl
  , FromJSON (Id (Endorser (Protocol impl)))
  ) => FromJSON (State sip impl)


-- | Reason why an implementation did not get adopted and was discarded.
data Reason
  = Expired
  -- ^ The candidate did not meet the adoption threshold at the end of the
  -- safety lag.
  | Canceled
  -- ^ The implementation was explicitly canceled by a cancellation proposal, or
  -- canceled by another with the same version.
  | Unsupported
  -- ^ If the version that the implementation supersedes can never be adopted.
  -- This can happen because the proposal supersedes a version lower than the
  -- current protocol version.
  --
  -- Note than when a version gets replaced, the replaced version in also marked
  -- as unsupported.
  deriving (Eq, Show, Generic, NFData, NoThunks, ToJSON, FromJSON)

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

deriving instance
  ( Eq (Application impl)
  , Implementation sip impl
  ) => Eq (MaybeAnEndorsedProposal sip impl)

deriving instance
  ( NFData (Protocol impl)
  , NFData (Id (Endorser (Protocol impl)))
  ) => NFData (MaybeAnEndorsedProposal sip impl)

deriving instance
  ( NoThunks (Protocol impl)
  , NoThunks (Id (Endorser (Protocol impl)))
  ) => NoThunks (MaybeAnEndorsedProposal sip impl)

deriving instance
  ( ToJSON (Protocol impl)
  , ToJSON (Id (Endorser (Protocol impl)))
  ) => ToJSON (MaybeAnEndorsedProposal sip impl)

deriving instance
  ( FromJSON (Protocol impl)
  , Activable (Protocol impl)
  , FromJSON (Id (Endorser (Protocol impl)))
  ) => FromJSON (MaybeAnEndorsedProposal sip impl)

initialState
  :: Implementation sip impl
  => Protocol impl
  -- ^ Initial protocol. This determines the current version.
  -> State sip impl
initialState initialProtocol =
  State
  { activationQueue    = mempty
  , endorsedProposal   = NoProposal
  , currentProtocol    = initialProtocol
  , applicationUpdates = mempty
  , lastAppliedSlot    = Nothing
  , discarded          = mempty
  }

--------------------------------------------------------------------------------
-- Operations on the proposals queue
--------------------------------------------------------------------------------

-- | Add the given version to the activation queue. If the queue contains a
-- protocol with the given version, then the version in the queue is discarded
-- due to being rendered obsolete by the newer proposal. The obsoleted proposal
-- is removed from the activation queue and marked as "discarded".
enqueue
  :: Implementation sip impl
  => Protocol impl -> State sip impl -> State sip impl
enqueue protocol st
  = enqueueUnchecked
  $ discardSameVersion st
  where
    discardSameVersion st' =
      case Map.lookup (version protocol) (activationQueue st') of
        Just protocol' ->
          st' { discarded = Map.insert protocol' Canceled (discarded st') }
        Nothing        ->
          st'

    enqueueUnchecked st' =
      st' { activationQueue =
            Map.insert (version protocol) protocol (activationQueue st')
          }

-- | Delete those proposals that cannot follow the current version.
--
-- A proposal can follow the current version if it supersedes a version higher
-- than the current version, or the version it supersedes is exactly the current
-- version.
deleteProposalsThatCannotFollow
  :: Implementation sip impl
  => State sip impl
  -> State sip impl
deleteProposalsThatCannotFollow st =
  discardProposalsThatDoNotSatisfy canFollow Unsupported st
  where
    canFollow protocol
      =  currentProtocolVersion st <  supersedesVersion protocol
      || _id (currentProtocol st)  == supersedesId protocol

-- | Discard a proposal, marking it as obsolete.
obsoleteProposal
  :: Implementation sip impl
  => Protocol impl -> State sip impl -> State sip impl
obsoleteProposal protocol st =
  st { discarded = Map.insert protocol Unsupported (discarded st)}

discardProposalsThatDoNotSatisfy
  :: Implementation sip impl
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
-- Operations on candidates
--------------------------------------------------------------------------------

-- | If there is a proposal in the queue that has the lowest version and can
-- follow the current version, make it a candidate.
--
-- This function needs to know the first slot of the next epoch and the number
-- of slots per epoch to be able to calculate when the safety lag of the
-- candidate will expire.
--
-- PRECONDITION: there must not be an endorsed or scheduled proposal.
findANewCandidate
  :: ( TracksSlotTime env
     , Implementation sip impl
     )
  => env
  -> State sip impl
  -> State sip impl
findANewCandidate env st
  = assert preconditionsHold
  $ case nextCandidate of
      Just nextProtocol ->
        st { activationQueue  =
               Map.delete (version nextProtocol) (activationQueue st)
           , endorsedProposal =
             Candidate
             { cProtocol       = nextProtocol
             , cEndorsements   = noEndorsements
             , cEndOfSafetyLag = endOfSafetyLag env
             }
           }
      Nothing           -> st
  where
    preconditionsHold = endorsedProtocol st ==! Nothing

    -- | Find the proposal with the lowest version that supersedes the given
    -- current version.
    nextCandidate =
      case Map.lookupMin (activationQueue st) of
        Just (_, minProtocol)
          | currentProtocolVersion st == supersedesVersion minProtocol
            && _id (currentProtocol st) == supersedesId minProtocol
            -> Just minProtocol
        _ -> Nothing

endOfSafetyLag :: TracksSlotTime env => env -> SlotNo
endOfSafetyLag env = nextEpochFirstSlot env + slotsPerEpoch env

-- | Discard a potential candidate. The reason is used to record why the
-- candidate was discarded. Return the state unaltered if there is no candidate.
discardCandidate
  :: Implementation sip impl
  => Reason -> State sip impl -> State sip impl
discardCandidate reason st =
  case endorsedProposal st of
    Candidate { cProtocol } -> st { endorsedProposal = NoProposal
                                  , discarded        =
                                      Map.insert cProtocol
                                                 reason
                                                 (discarded st)
                                  }
    _                       -> st

-- | Put a candidate back in the queue.
reEnqueueCandidate
  :: Implementation sip impl => State sip impl -> State sip impl
reEnqueueCandidate st =
  case endorsedProposal st of
    Candidate { cProtocol }
      -> st { activationQueue  =
                Map.insert (version cProtocol)
                           cProtocol
                           (activationQueue st)
            , endorsedProposal = NoProposal
            }
    _ -> st

-- | Mark a potential candidate as scheduled. Return the state unaltered if
-- there is no candidate.
scheduleCandidate
  :: State sip impl -> State sip impl
scheduleCandidate st =
  case endorsedProposal st of
    Candidate { cProtocol } -> st { endorsedProposal =
                                      Scheduled { sProtocol = cProtocol }
                                  }
    _                       -> st

-- | If there is a scheduled proposal activate it. This means that the scheduled
-- proposal becomes the current version and there is no endorsed proposal in the
-- returned state.
activate
  :: Implementation sip impl
  => State sip impl -> State sip impl
activate st =
  case endorsedProposal st of
    Scheduled { sProtocol }
      -> deleteProposalsThatCannotFollow
         $ obsoleteProposal (currentProtocol st)
         $ st { endorsedProposal = NoProposal
              , currentProtocol  = sProtocol
              }
    _ -> st

--------------------------------------------------------------------------------
-- Operations on applications
--------------------------------------------------------------------------------

addApplication
  :: Implementation sip impl
  => Application impl -> State sip impl -> State sip impl
addApplication application st
  = assert preconditionsHold
  $ st { applicationUpdates = application : (applicationUpdates st) }
  where
    preconditionsHold =
      fmap _id (applicationUpdates st) `doesNotContain` _id application

--------------------------------------------------------------------------------
-- Operations on endorsements
--------------------------------------------------------------------------------

endorseInThisEpoch
  :: Implementation sip impl
  => EndorserId (Protocol impl) -> State sip impl -> State sip impl
endorseInThisEpoch endorserId st =
  endorseWith (addEndorsementInThisEpoch endorserId) st

endorseInNextEpoch
  :: Implementation sip impl
  => EndorserId (Protocol impl) -> State sip impl -> State sip impl
endorseInNextEpoch endorserId st =
  endorseWith (addEndorsementInNextEpoch endorserId) st

endorseWith
  :: (Endorsements impl -> Endorsements impl)
  -> State sip impl -> State sip impl
endorseWith f st =
  case endorsedProposal st of
    Candidate { cProtocol, cEndorsements, cEndOfSafetyLag }
      -> st { endorsedProposal =
                Candidate
                { cProtocol = cProtocol
                , cEndorsements   = f cEndorsements
                , cEndOfSafetyLag = cEndOfSafetyLag
                }
            }
    _ -> error "No candidate to endorse"

-- | If there is a candidate transfer its next epoch endorsements to the current
-- epoch endorsements.
transferEndorsements
  :: Implementation sip impl
  => State sip impl -> State sip impl
transferEndorsements st =
  case endorsedProposal st of
    Candidate { cProtocol, cEndorsements, cEndOfSafetyLag }
      -> st { endorsedProposal =
                Candidate
                { cProtocol = cProtocol
                , cEndorsements   = changeEpoch cEndorsements
                , cEndOfSafetyLag = cEndOfSafetyLag
                }
            }
    _ -> st

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

deriving instance Activable (Protocol impl) => Eq (Endorsements impl)

deriving instance
  ( NFData (Id (Endorser (Protocol impl)))
  ) => NFData (Endorsements impl)

deriving instance
  ( NoThunks (Id (Endorser (Protocol impl)))
  ) => NoThunks (Endorsements impl)

deriving instance
  ( ToJSON (Id (Endorser (Protocol impl)))
  ) => ToJSON (Endorsements impl)

deriving instance
  ( Activable (Protocol impl)
  , FromJSON (Id (Endorser (Protocol impl)))
  ) => FromJSON (Endorsements impl)

--------------------------------------------------------------------------------
-- Internal operations on endorsements
--------------------------------------------------------------------------------

noEndorsements :: Activable (Protocol impl) => Endorsements impl
noEndorsements =
  Endorsements
  { thisEpochEndorsements = mempty
  , nextEpochEndorsements = mempty
  }

addEndorsementInThisEpoch
  :: Activable (Protocol impl)
  => EndorserId (Protocol impl)
  -> Endorsements impl
  -> Endorsements impl
addEndorsementInThisEpoch endorserId endorsements =
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

addEndorsementInNextEpoch
  :: Activable (Protocol impl)
  => EndorserId (Protocol impl)
  -> Endorsements impl
  -> Endorsements impl
addEndorsementInNextEpoch endorserId endorsements =
  endorsements { nextEpochEndorsements =
                   Set.insert endorserId (nextEpochEndorsements endorsements)
               }

--------------------------------------------------------------------------------
-- Operations on the last applied slot
--------------------------------------------------------------------------------

tickLastAppliedSlot
  :: SlotNo -> State sip impl -> State sip impl
tickLastAppliedSlot slot st
  = st { lastAppliedSlot = Just slot }

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

getCurrentProtocolId
  :: HasActivationState st sip impl => st -> ProtocolId impl
getCurrentProtocolId = _id  . getCurrentProtocol

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
  :: HasActivationState st sip impl
  => ProtocolId impl -> st -> Bool
isTheCurrentVersion h = (== h) . _id . currentProtocol . getActivationState

isScheduled
  :: HasActivationState st sip impl
  => ProtocolId impl -> st -> Bool
isScheduled h =
  fromMaybe False . fmap (== h) . scheduledProtocolId . getActivationState

isBeingEndorsed
  :: HasActivationState st sip impl
  => ProtocolId impl -> st -> Bool
isBeingEndorsed h =
  fromMaybe False . fmap (== h) . endorsedProtocolId . getActivationState

isQueued
  :: HasActivationState st sip impl
  => ProtocolId impl -> st -> Bool
isQueued h st = h `elem` fmap _id queuedImplementations
  where
    queuedImplementations = Map.elems $ activationQueue $ getActivationState st

isDiscardedDueToBeing
  :: HasActivationState st sip impl
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

-- | Return the candidate protocol (if any). If there a candidate is scheduled
-- this function will return 'Nothing'.
candidateProtocol
  :: HasActivationState st sip impl => st -> Maybe (Protocol impl)
candidateProtocol st =
  case endorsedProposal (getActivationState st) of
    Candidate { cProtocol } -> Just cProtocol
    _                       -> Nothing

scheduledProtocol
  :: HasActivationState st sip impl => st -> Maybe (Protocol impl)
scheduledProtocol st =
  case endorsedProposal (getActivationState st) of
    Scheduled { sProtocol } -> Just sProtocol
    _                       -> Nothing

scheduledProtocolVersion
  :: HasActivationState st sip impl => st -> Maybe (Version (Protocol impl))
scheduledProtocolVersion = fmap version . scheduledProtocol

endorsedProtocolVersion
  :: HasActivationState st sip impl => st -> Maybe (Version (Protocol impl))
endorsedProtocolVersion
  = fmap version . endorsedProtocol

endorsedProtocolId
  :: HasActivationState st sip impl => st -> Maybe (ProtocolId impl)
endorsedProtocolId =
    fmap _id . endorsedProtocol

endorsedSupersedesVersion
  :: HasActivationState st sip impl => st -> Maybe (Version (Protocol impl))
endorsedSupersedesVersion =
  fmap supersedesVersion . endorsedProtocol

currentProtocolVersion
  :: HasActivationState st sip impl => st -> Version (Protocol impl)
currentProtocolVersion =
  version  . currentProtocol . getActivationState

scheduledProtocolId
  :: HasActivationState st sip impl => st -> Maybe (ProtocolId impl)
scheduledProtocolId
  = fmap _id . scheduledProtocol . getActivationState

queuedProtocols :: HasActivationState st sip impl => st -> [Protocol impl]
queuedProtocols = Map.elems . activationQueue . getActivationState

-- | Return the queued protocols (see 'queuedProtocols') and any eventual
-- candidate.
candidateProtocols :: HasActivationState st sip impl => st -> [Protocol impl]
candidateProtocols st = maybeToList (endorsedProtocol st) ++ queuedProtocols st

--------------------------------------------------------------------------------
-- Serialisation instances
--------------------------------------------------------------------------------

instance
  ( Typeable sip
  , Typeable impl
  , Activable (Protocol impl)
  , ToCBOR (Protocol impl)
  , ToCBOR (Version (Protocol impl))
  , ToCBOR (Application impl)
  , ToCBOR (Id (Endorser (Protocol impl)))
  ) => ToCBOR (State sip impl) where
  toCBOR st
    =  encodeListLen 6
    <> toCBOR (endorsedProposal st)
    <> toCBOR (currentProtocol st)
    <> toCBOR (activationQueue st)
    <> toCBOR (applicationUpdates st)
    <> toCBOR (lastAppliedSlot st)
    <> toCBOR (discarded st)

instance
  ( Typeable sip
  , Typeable impl
  , Activable (Protocol impl)
  , FromCBOR (Protocol impl)
  , FromCBOR (Version (Protocol impl))
  , FromCBOR (Application impl)
  , FromCBOR (Id (Endorser (Protocol impl)))
  ) => FromCBOR (State sip impl) where
  fromCBOR = do
    decodeListLenOf 6
    ep <- fromCBOR
    cp <- fromCBOR
    aq <- fromCBOR
    au <- fromCBOR
    la <- fromCBOR
    di <- fromCBOR
    return $! State ep cp aq au la di

instance
  ( Typeable sip
  , Typeable impl
  , Activable (Protocol impl)
  , ToCBOR (Protocol impl)
  , ToCBOR (Id (Endorser (Protocol impl)))
  ) => ToCBOR (MaybeAnEndorsedProposal sip impl) where
  toCBOR NoProposal     =  encodeListLen 0
  toCBOR c@Candidate {} =  encodeListLen 3
                        <> toCBOR (cProtocol c)
                        <> toCBOR (cEndorsements c)
                        <> toCBOR (cEndOfSafetyLag c)
  toCBOR s@Scheduled {} =  encodeListLen 1
                        <> toCBOR (sProtocol s)

instance
  ( Typeable sip
  , Typeable impl
  , Activable (Protocol impl)
  , FromCBOR (Protocol impl)
  , FromCBOR (Id (Endorser (Protocol impl)))
  ) => FromCBOR (MaybeAnEndorsedProposal sip impl) where
  fromCBOR = do
    n <- decodeListLen
    case n of
      0 -> return $! NoProposal
      1 -> do
        !s <- fromCBOR
        return $! Scheduled s
      3 -> do
        !pr <- fromCBOR
        !en <- fromCBOR
        !es <- fromCBOR
        return $! Candidate pr en es
      _ -> fail $  "Unknown tag when decoding a value of type 'MaybeAnEndorsedProposal'"
                <> show n

instance
  ( Typeable impl
  , Activable (Protocol impl)
  , ToCBOR (Id (Endorser (Protocol impl)))
  ) => ToCBOR (Endorsements impl) where
  toCBOR en
    =  encodeListLen 2
    <> toCBOR (thisEpochEndorsements en)
    <> toCBOR (nextEpochEndorsements en)

instance
  ( Typeable impl
  , Activable (Protocol impl)
  , FromCBOR (Id (Endorser (Protocol impl)))
  ) => FromCBOR (Endorsements impl) where
  fromCBOR = do
    decodeListLenOf 2
    t <- fromCBOR
    n <- fromCBOR
    return $! Endorsements t n

reasonEncoding :: [(Int, Reason)]
reasonEncoding = [ (0, Expired)
                 , (1, Canceled)
                 , (2, Unsupported)
                 ]

instance ToCBOR Reason where
  toCBOR r =
    case find ((== r) . snd) reasonEncoding of
      Nothing     -> error $ "Reason " <> show r <> " is not in the encoding map"
      Just (i, _) -> encodeInt i


instance FromCBOR Reason where
  fromCBOR = do
    i <- decodeInt
    case lookup i reasonEncoding of
      Nothing -> fail $  "Decoded integer value '" <> show i
                      <> "' is an invalid encoding of a value of type 'Reason'"
      Just r  -> return $! r
