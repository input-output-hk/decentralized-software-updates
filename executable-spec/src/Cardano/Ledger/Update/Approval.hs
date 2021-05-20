{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Approval phase interface
--
module Cardano.Ledger.Update.Approval
  ( State
  , Error
    -- * State update functions
  , initialState
  , tick
  , apply
    -- * State query functions
  , HasApprovalState (getApprovalState)
  , removeApproved
  , isImplementation
  , isImplementationStably
  , isImplementationSubmitted
  , isImplementationStablySubmitted
  , isImplementationRevealed
  , isImplementationStablyRevealed
  -- * Error query
  , HasApprovalError (getApprovalError)
  , noApprovedSIP
  , noImplementationCommit
  , noStableImplementationCommit
  , implementationVotePeriodHasNotStarted
  , implementationVotePeriodHasEnded
  )
where

import           Control.DeepSeq (NFData)
import           Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Control.Monad (unless, when)
import           Control.Monad.Except (throwError)
import           Data.Map.Strict (Map)
import           Data.Maybe (isJust)
import           Data.Set (Set)
import           Data.Typeable (Typeable)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Cardano.Slotting.Slot (SlotNo)
import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR),
                     decodeListLenOf, encodeListLen)

import           Cardano.Ledger.Update.Env.HasAdversarialStakeRatio
                     (HasAdversarialStakeRatio)
import           Cardano.Ledger.Update.Env.HasStakeDistribution
                     (HasStakeDistribution)
import           Cardano.Ledger.Update.Env.HasVotingPeriodsCap
                     (HasVotingPeriodsCap, maxVotingPeriods)
import           Cardano.Ledger.Update.Env.TracksSlotTime (TracksSlotTime,
                     currentSlot, stableAt)

import qualified Cardano.Ledger.Update.Ideation as Ideation

import           Cardano.Ledger.Update.ProposalsState (ProposalsState)
import qualified Cardano.Ledger.Update.ProposalsState as Proposals
import           Cardano.Ledger.Update.ProposalState (Decision)
import           Cardano.Ledger.Update.ProposalState (Decision (Approved))

import           Cardano.Ledger.Update.Proposal hiding (commit)
import qualified Cardano.Ledger.Update.Proposal as Proposal

data State impl =
  State
  { submissionStableAt :: !(Map (Commit (Revelation impl)) SlotNo)
    -- ^ Submitted commits, along with the timestamp (slot) at which they were
    -- submitted.
  , proposalsState     :: !(ProposalsState impl)
    -- ^ Implementation-proposals state. See 'ProposalsState'.
  , movedAway          :: !(Set (Id impl))
    -- ^ Implementations that moved away from this phase. We keep track of this
    -- to detect duplicated revelations of proposals that already moved away
    -- from this phase.
  } deriving (Generic)

deriving instance Proposal impl => Show (State impl)

deriving instance (Proposal impl, Eq impl) => Eq (State impl)

deriving instance
  ( NFData impl
  , NFData (Id impl)
  , NFData (Id (Voter impl))
  , NFData (Commit (Revelation impl))
  ) => NFData (State impl)

deriving instance
  ( NoThunks impl
  , NoThunks (Id impl)
  , NoThunks (Voter impl)
  , NoThunks (Id (Voter impl))
  , NoThunks (Commit (Revelation impl))
  ) => NoThunks (State impl)

deriving instance
  ( ToJSON impl
  , ToJSON (Id impl)
  , ToJSONKey (Id impl)
  , ToJSONKey (Id (Voter impl))
  , ToJSONKey (Commit (Revelation impl))
  ) => ToJSON (State impl)

deriving instance
  ( Proposal impl
  , FromJSON (impl)
  , FromJSON (Id impl)
  , FromJSONKey (Id impl)
  , FromJSONKey (Id (Voter impl))
  , FromJSONKey (Commit (Revelation impl))
  ) => FromJSON (State impl)

data Error sip impl
  = ImplementationCommitAlreadySubmitted (Commit (Revelation impl))
  | CommitSignatureDoesNotVerify (Commit (Revelation impl))
  | NoCorrespondingCommit (Revelation impl)
  | NoStableCommit (Revelation impl)
  | ImplementationAlreadyRevealed (Revelation impl)
  | NoApprovedSIP (Id sip)
  | VoteSignatureDoesNotVerify (Vote impl)
  | VotedProposalDoesNotExist (Vote impl)
  | VotePeriodHasNotStarted SlotNo (Vote impl) (ProposalsState impl)
  | VotePeriodHasEnded SlotNo (Vote impl) (ProposalsState impl)

deriving instance Implementation sip impl
                  => Show (Error sip impl)

class HasApprovalError err sip impl
     | err -> sip, err -> impl      where
  getApprovalError :: err -> Maybe (Error sip impl)

instance HasApprovalError (Error sip impl) sip impl where
  getApprovalError = Just . id

noImplementationCommit
  :: HasApprovalError err sip impl => err -> Maybe (Revelation impl)
noImplementationCommit err = do
  apprvErr <- getApprovalError err
  case apprvErr of
    NoCorrespondingCommit impl -> Just impl
    _                          -> Nothing

noStableImplementationCommit
  :: HasApprovalError err sip impl => err -> Maybe (Revelation impl)
noStableImplementationCommit err = do
  apprvErr <- getApprovalError err
  case apprvErr of
    NoStableCommit impl -> Just impl
    _                   -> Nothing

noApprovedSIP
  :: HasApprovalError err sip impl => err -> Maybe (Id sip)
noApprovedSIP err = do
  apprvErr <- getApprovalError err
  case apprvErr of
    NoApprovedSIP sipHash -> Just sipHash
    _                     -> Nothing

implementationVotePeriodHasNotStarted
  :: (Implementation sip impl, HasApprovalError err sip impl) => err -> Maybe (Id impl)
implementationVotePeriodHasNotStarted err = do
  apprvErr <- getApprovalError err
  case apprvErr of
    VotePeriodHasNotStarted _ vote _ -> Just (candidate vote)
    _                                -> Nothing

implementationVotePeriodHasEnded
  :: (Implementation sip impl, HasApprovalError err sip impl) => err -> Maybe (Id impl)
implementationVotePeriodHasEnded err = do
  apprvErr <- getApprovalError err
  case apprvErr of
    VotePeriodHasEnded _ vote _ -> Just (candidate vote)
    _                           -> Nothing

--------------------------------------------------------------------------------
-- State update functions
--------------------------------------------------------------------------------

initialState :: Proposal impl => State impl
initialState =
  State
  { submissionStableAt = mempty
  , proposalsState     = Proposals.initialState
  , movedAway          = mempty
  }

tick
  :: ( HasAdversarialStakeRatio env
     , HasStakeDistribution  env (Id (Voter impl))
     , TracksSlotTime env
     , Proposal impl
     )
   => env -> State impl -> State impl
tick env st =
  st { proposalsState = Proposals.tally env (proposalsState st) }

apply
  :: ( HasVotingPeriodsCap env
     , TracksSlotTime env

     , Proposal sip
     , Implementation sip impl
     )
  => env
  -> Ideation.State sip
  -> Payload impl
  -> State impl
  -> Either (Error sip impl) (State impl)
apply env _ideationSt (Submit submission) st = do
  let commit = revelationCommit submission
  when (isImplementationSubmitted commit st)
    $ throwError (ImplementationCommitAlreadySubmitted commit)
  unless (signatureVerifies submission)
    $ throwError (CommitSignatureDoesNotVerify commit)
  pure $ st { submissionStableAt =
                Map.insert commit (stableAt env (currentSlot env)) (submissionStableAt st)
            }
apply env ideationSt (Reveal revelation) st  = do
  let commit = Proposal.commit revelation
  unless (isImplementationSubmitted commit st)
    $ throwError (NoCorrespondingCommit revelation)
  unless (isImplementationStablySubmitted env commit st)
    $ throwError (NoStableCommit revelation)
  let implementationId = _id (proposal revelation)
      implementedSIPId = preProposalId (proposal revelation)
  when (  isImplementationRevealed implementationId st
       || implementationId `Set.member` movedAway st
       )
    $ throwError (ImplementationAlreadyRevealed revelation)
  unless (Ideation.isSIP implementedSIPId Approved ideationSt)
    $ throwError (NoApprovedSIP implementedSIPId)
  pure $ st { proposalsState =
                Proposals.reveal (currentSlot env)
                                 (maxVotingPeriods env)
                                 (proposal revelation)
                                 (proposalsState st)
            }
apply env _ideationSt (Cast vote) st         = do
  unless (signatureVerifies vote)
    $ throwError (VoteSignatureDoesNotVerify vote)
  let implementationId = candidate vote
  unless (Proposals.isRevealed implementationId (proposalsState st))
    $ throwError (VotedProposalDoesNotExist vote)
  unless (Proposals.votingPeriodHasStarted env implementationId (proposalsState st))
    $ throwError (VotePeriodHasNotStarted (currentSlot env)
                                          vote
                                          (proposalsState st)
                 )
  when (Proposals.votingPeriodHasEnded env implementationId (proposalsState st))
    $ throwError (VotePeriodHasEnded (currentSlot env)
                                     vote
                                     (proposalsState st)
                 )
  pure $ st { proposalsState =
              Proposals.updateBallot implementationId vote (proposalsState st)
            }

removeApproved
  :: Identifiable impl
  => State impl
  -> ([impl], State impl)
removeApproved st =
  ( approvedImpls
  , st { proposalsState = proposalState'
       , movedAway      = Set.fromList (fmap _id approvedImpls)
                          `Set.union`
                          movedAway st
       }
  )
  where
    (approvedImpls, proposalState') =
      Proposals.removeApproved (proposalsState st)

--------------------------------------------------------------------------------
-- State query functions
--------------------------------------------------------------------------------

class HasApprovalState st impl | st -> impl where
  getApprovalState :: st -> State impl

instance HasApprovalState (State impl) impl where
  getApprovalState = id

isImplementationSubmitted
  :: (Proposal impl, HasApprovalState st impl)
  => Commit (Revelation impl) -> st -> Bool
isImplementationSubmitted commit =
  isJust . Map.lookup commit . submissionStableAt . getApprovalState

isImplementationStablySubmitted
  :: ( Proposal impl
     , TracksSlotTime env
     , HasApprovalState st impl
     )
  => env -> Commit (Revelation impl) -> st -> Bool
isImplementationStablySubmitted env commit
  = maybe False (<= currentSlot env)
  . Map.lookup commit
  . submissionStableAt
  . getApprovalState

isImplementationRevealed
  :: (Proposal impl, HasApprovalState st impl)
  => Id impl -> st -> Bool
isImplementationRevealed implementationId =
  Proposals.isRevealed implementationId . proposalsState . getApprovalState

isImplementationStablyRevealed
  :: ( Proposal impl
     , TracksSlotTime env
     , HasApprovalState st impl
     )
  => env -> Id impl -> st -> Bool
isImplementationStablyRevealed env implementationId =
  Proposals.isStablyRevealed env implementationId . proposalsState . getApprovalState

isImplementation
  :: (Proposal impl, HasApprovalState st impl)
  => Id impl -> Decision -> st -> Bool
isImplementation implementationId d =
  Proposals.is implementationId d . proposalsState . getApprovalState

isImplementationStably
  :: ( Proposal impl
     , TracksSlotTime env
     , HasApprovalState st impl
     )
  => env -> Id impl -> Decision -> st -> Bool
isImplementationStably env implementationId d =
  Proposals.isStably env implementationId d . proposalsState . getApprovalState

--------------------------------------------------------------------------------
-- Serialisation instances
--------------------------------------------------------------------------------

instance
  ( Typeable impl
  , Proposal impl
  , ToCBOR impl
  , ToCBOR (Id impl)
  , ToCBOR (Id (Voter impl))
  , ToCBOR (Commit (Revelation impl))
  ) => ToCBOR (State impl) where
  toCBOR st =  encodeListLen 3
            <> toCBOR (submissionStableAt st)
            <> toCBOR (proposalsState st)
            <> toCBOR (movedAway st)

instance
  ( Typeable impl
  , Proposal impl
  , FromCBOR impl
  , FromCBOR (Id impl)
  , FromCBOR (Id (Voter impl))
  , FromCBOR (Commit (Revelation impl))
  ) => FromCBOR (State impl) where
  fromCBOR = do
    decodeListLenOf 3
    ss <- fromCBOR
    ps <- fromCBOR
    ma <- fromCBOR
    return $! State ss ps ma
