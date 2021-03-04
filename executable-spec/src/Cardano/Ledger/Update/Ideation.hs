{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Ideation phase interface.
--
-- This module is intended to be used qualified.
--
module Cardano.Ledger.Update.Ideation
  ( State
  , Error
    -- * State update functions
  , initialState
  , tick
  , apply
    -- * State query functions
  , HasIdeationState (getIdeationState)
  , isSIPSubmitted
  , isSIPStablySubmitted
  , isSIPRevealed
  , isSIPStablyRevealed
  , isSIP
  , isSIPStably
  )
where

import           Control.DeepSeq (NFData)
import           Control.Monad (unless, when)
import           Control.Monad.Except (throwError)
import           Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR),
                     decodeListLenOf, encodeListLen)
import           Cardano.Slotting.Slot (SlotNo)

import           Cardano.Ledger.Update.Env.HasAdversarialStakeRatio
                     (HasAdversarialStakeRatio)
import           Cardano.Ledger.Update.Env.HasStakeDistribution
                     (HasStakeDistribution)
import           Cardano.Ledger.Update.Env.HasVotingPeriodsCap
                     (HasVotingPeriodsCap, maxVotingPeriods)
import           Cardano.Ledger.Update.Env.TracksSlotTime (TracksSlotTime,
                     currentSlot, stableAt)
import           Cardano.Ledger.Update.ProposalState (Decision)

import           Cardano.Ledger.Update.ProposalsState (ProposalsState)
import qualified Cardano.Ledger.Update.ProposalsState as Proposals


-- TODO: import qualified
import           Cardano.Ledger.Update.Proposal hiding (commit)
import qualified Cardano.Ledger.Update.Proposal as Proposal

-- | Ideation state parameterized over the ideation payload.
data State p =
  State
  { submissionStableAt :: !(Map (Commit (Revelation p)) SlotNo)
  , proposalsState     :: !(ProposalsState p)
  } deriving (Generic)

deriving instance Proposal p => Show (State p)

deriving instance
  ( NFData p, NFData (Commit (Revelation p)), NFData (Id p)
  , NFData (Id (Voter p))
  ) =>
  NFData (State p)

deriving instance
  ( NoThunks p, NoThunks (Commit (Revelation p)), NoThunks (Voter p)
  , NoThunks (Id p), NoThunks (Id (Voter p))
  ) =>
  NoThunks (State p)

deriving instance
  ( ToJSONKey (Commit (Revelation p)), ToJSONKey (Id p)
  , ToJSONKey (Id (Voter p)), ToJSON p
  ) =>
  ToJSON (State p)

deriving instance
  ( Proposal p, FromJSON p, FromJSONKey (Commit (Revelation p))
  , FromJSONKey (Id p), FromJSONKey (Id (Voter p))
  ) =>
  FromJSON (State p)

data Error p
  = SIPCommitAlreadySubmitted (Submission p)
  | CommitSignatureDoesNotVerify (Submission p)
  | NoCorrespondingCommit (Revelation p)
  | NoStableCommit (Revelation p)
  | AlreadyRevealed (Revelation p)
  | VoteSignatureDoesNotVerify (Vote p)
  | VotePeriodHasNotStarted SlotNo (Vote p) (ProposalsState p)
  | VotePeriodHasEnded SlotNo (Vote p) (ProposalsState p)

deriving instance Proposal p => Show (Error p)

--------------------------------------------------------------------------------
-- State update functions
--------------------------------------------------------------------------------

initialState :: Proposal p => State p
initialState =
  State
  { submissionStableAt = mempty
  , proposalsState     = Proposals.initialState
  }

-- | Tick to a target slot. The environment contains the slot we're ticking to.
--
-- TODO: this function relies on the fact that we tick at each slot. We could
-- check this by adding the last applied slot to the state, but this would
-- duplicate state and checks. It is not clear right not if this is a compromise
-- worth doing.
tick
  :: ( TracksSlotTime env
     , HasAdversarialStakeRatio env
     , HasStakeDistribution env (VoterId p)
     , Proposal p
     )
  => env -> State p -> State p
tick env st =
  st { proposalsState = Proposals.tally env (proposalsState st) }

apply
  :: ( TracksSlotTime env
     , HasVotingPeriodsCap env

     , Proposal p
     )
  => env -> Payload p -> State p -> Either (Error p) (State p)
apply env (Submit submission) st = do
  let commit = revelationCommit submission
  when (isSIPSubmitted commit st)
    $ throwError (SIPCommitAlreadySubmitted submission)
  unless (signatureVerifies submission)
    $ throwError (CommitSignatureDoesNotVerify submission)
  pure $ st { submissionStableAt =
              Map.insert commit
                         (stableAt env (currentSlot env))
                         (submissionStableAt st)
            }
apply env (Reveal revelation) st  = do
  let commit = Proposal.commit revelation
  unless (isSIPSubmitted commit st)
    $ throwError (NoCorrespondingCommit revelation)
  unless (isSIPStablySubmitted env commit st)
    $ throwError (NoStableCommit revelation)
  let sipId  = _id (proposal revelation)
  when (isSIPRevealed sipId st)
    $ throwError (AlreadyRevealed revelation)
  pure $ st { proposalsState =
                Proposals.reveal (currentSlot env)
                                 (maxVotingPeriods env)
                                 (proposal revelation)
                                 (proposalsState st)
            }
apply env (Cast vote) st   = do
  unless (signatureVerifies vote)
    $ throwError (VoteSignatureDoesNotVerify vote)
  let sipId = candidate vote
  unless (Proposals.votingPeriodHasStarted env sipId (proposalsState st))
    $ throwError (VotePeriodHasNotStarted (currentSlot env) vote (proposalsState st))
  when (Proposals.votingPeriodHasEnded env sipId (proposalsState st))
    $ throwError (VotePeriodHasEnded (currentSlot env) vote (proposalsState st))
  pure $ st { proposalsState =
              Proposals.updateBallot sipId vote (proposalsState st)
            }

--------------------------------------------------------------------------------
-- State query functions
--------------------------------------------------------------------------------

class HasIdeationState st p | st -> p where
  getIdeationState :: st -> State p

instance HasIdeationState (State p) p where
  getIdeationState = id

isSIPSubmitted
  :: (Proposal p, HasIdeationState st p)
  => Commit (Revelation p)
  -> st
  -> Bool
isSIPSubmitted sipCommit =
  Map.member sipCommit . submissionStableAt . getIdeationState

isSIPStablySubmitted
  :: ( Proposal p
     , TracksSlotTime env
     , HasIdeationState st p
     )
  => env
  -> Commit (Revelation p)
  -> st
  -> Bool
isSIPStablySubmitted env sipCommit
  = maybe False (<= currentSlot env)
  . Map.lookup sipCommit
  . submissionStableAt
  . getIdeationState

isSIPRevealed
  :: ( Proposal p
     , HasIdeationState st p
     )
  => Id p -> st -> Bool
isSIPRevealed sipDataHash =
  Proposals.isRevealed sipDataHash . proposalsState . getIdeationState

isSIPStablyRevealed
  :: ( Proposal p
     , TracksSlotTime env
     , HasIdeationState st p
     )
  => env -> Id p -> st -> Bool
isSIPStablyRevealed env sipDataHash =
  Proposals.isStablyRevealed env sipDataHash . proposalsState . getIdeationState

isSIP
  :: ( Proposal p
     , HasIdeationState st p
     )
  => Id p -> Decision -> st -> Bool
isSIP sipDataHash d =
  Proposals.is sipDataHash d . proposalsState . getIdeationState

isSIPStably
  :: ( Proposal p
     , TracksSlotTime env
     , HasIdeationState st p
     )
  => env -> Id p -> Decision -> st -> Bool
isSIPStably env sipDataHash d =
  Proposals.isStably env sipDataHash d . proposalsState . getIdeationState

--------------------------------------------------------------------------------
-- Serialisation instances
--------------------------------------------------------------------------------

instance
  ( Typeable p
  , ToCBOR (Commit (Revelation p))
  , Proposal p
  , ToCBOR p
  , ToCBOR (Id p)
  , ToCBOR (Id (Voter p))
  ) => ToCBOR (State p) where
  toCBOR st
    =  encodeListLen 2
    <> toCBOR (submissionStableAt st)
    <> toCBOR (proposalsState st)

instance
  ( Typeable p
  , FromCBOR p
  , Proposal p
  , FromCBOR (Id (Voter p))
  , FromCBOR (Commit (Revelation p))
  , FromCBOR (Id p)
  ) => FromCBOR (State p) where
  fromCBOR = do
    decodeListLenOf 2
    ss <- fromCBOR
    ps <- fromCBOR
    return $! State ss ps
