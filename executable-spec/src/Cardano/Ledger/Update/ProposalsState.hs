{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Information about different proposals in the update system.
--
module Cardano.Ledger.Update.ProposalsState
  ( ProposalsState
  , initialState
  , tally
  , reveal
  , updateBallot
  , updateBallot'
  , votingPeriodHasStarted

  , votingPeriodHasEnded
  , votingPeriodHasNotEnded
  , decision
  , removeApproved
  -- * Proposal state query operations
  , isRevealed
  , isRevealed'
  , isNotRevealed'
  , isStablyRevealed
  , is
  , isStably
  -- ** @ProposalState@ re-exports
  , ProposalState.Decision ( Approved
                           , Expired
                           , Rejected
                           , Undecided
                           , WithNoQuorum
                           )
  , ProposalState.VotingPeriod (VotingPeriod, unVotingPeriod)
  )
where

import           Control.DeepSeq (NFData)
import           Control.Exception (assert)
import           Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Slotting.Slot (SlotNo)

import           Cardano.Ledger.Update.Env.HasAdversarialStakeRatio
                     (HasAdversarialStakeRatio)
import           Cardano.Ledger.Update.Env.HasStakeDistribution
                     (HasStakeDistribution)
import           Cardano.Ledger.Update.Env.TracksSlotTime (TracksSlotTime,
                     isStable)

import           Cardano.Ledger.Update.ProposalState (Decision (Approved),
                     ProposalState, VotingPeriod, newProposalState)
import qualified Cardano.Ledger.Update.ProposalState as ProposalState

import           Cardano.Ledger.Update.Proposal (Confidence, Id, Proposal, Vote,
                     Voter, _id)

-- | State of a proposal. The @p@ parameter determines the type of proposals.
newtype ProposalsState p =
  ProposalsState { proposalStateMap :: (Map (Id p) (ProposalState p)) }
  deriving (Show, Generic, Eq)

deriving instance ( NoThunks (Id p)
         , NoThunks (Voter p)
         , NoThunks (Id (Voter p))
         , NoThunks p
         ) => NoThunks (ProposalsState p)

deriving instance (NFData p, NFData (Id p), NFData (Id (Voter p)))
  => NFData (ProposalsState p)

deriving instance (ToJSON p, ToJSONKey (Id p), ToJSONKey (Id (Voter p))) =>
  ToJSON (ProposalsState p)

deriving instance (Proposal p, FromJSON p, FromJSONKey (Id p), FromJSONKey (Id (Voter p))) =>
  FromJSON (ProposalsState p)

initialState :: Proposal p => ProposalsState p
initialState = ProposalsState mempty

lookupProposal
  :: Proposal p
  => Id p -> ProposalsState p -> Maybe (ProposalState p)
lookupProposal dHash = Map.lookup dHash . proposalStateMap

-- | Tally each proposal in the state.
--
tally
  :: ( TracksSlotTime env
     , HasStakeDistribution env (Id (Voter p))
     , HasAdversarialStakeRatio env
     , Proposal p
     )
  => env
  -> ProposalsState p
  -> ProposalsState p
tally env ProposalsState { proposalStateMap }
  = ProposalsState
  $ fmap (ProposalState.tally env) proposalStateMap

-- | Register the revelation of a proposal.
--
reveal
  :: Proposal p
  => SlotNo
  -> VotingPeriod
  -> p
  -- ^ Proposal being revealed.
  -> ProposalsState p
  -> ProposalsState p
reveal currentSlot dMaxVotingPeriods p ProposalsState { proposalStateMap }
  = ProposalsState
  $ Map.insert (_id p) (newProposalState currentSlot dMaxVotingPeriods p) proposalStateMap

-- | Is the proposal revealed.
isRevealed' :: Proposal p => p -> ProposalsState p -> Bool
isRevealed' p ProposalsState { proposalStateMap }
  = Map.member (_id p) proposalStateMap

isNotRevealed' :: Proposal p => p -> ProposalsState p -> Bool
isNotRevealed' p st = not $ isRevealed' p st

-- | Register the vote for the given proposal's hash.
--
-- The proposal must be present in the map, otherwise an 'AssertionFailed'
-- exception will be thrown, unless the code is compiles with assertions
-- disabled. See 'Control.Exception.assert'.
--
-- TODO: we should remove this in favor of 'updateBallot''.
updateBallot
  :: Proposal p
  => Id p
  -> Vote p
  -> ProposalsState p
  -> ProposalsState p
updateBallot proposalId vote ProposalsState { proposalStateMap }
  =  assert (proposalId `Map.member` proposalStateMap)
  $  ProposalsState
  $! Map.adjust (ProposalState.updateBallot vote) proposalId proposalStateMap

updateBallot'
  :: Proposal p
  => Id p
  -> Id (Voter p)
  -> Confidence
  -> ProposalsState p
  -> ProposalsState p
updateBallot' proposalId voterId confidence st
  =  assert (proposalId `Map.member` proposalStateMap st)
  $  ProposalsState
  $! Map.adjust (ProposalState.updateBallot' voterId confidence) proposalId
  $  proposalStateMap st

votingPeriodHasStarted
  :: ( TracksSlotTime env
     , Proposal p
     )
  => env
  -> Id p
  -> ProposalsState p
  -> Bool
votingPeriodHasStarted env proposalId
  = maybe False (ProposalState.votingPeriodHasStarted env)
  . lookupProposal proposalId

votingPeriodHasEnded
  :: ( TracksSlotTime env
     , Proposal p
     )
  => env
  -> Id p
  -> ProposalsState p -> Bool
votingPeriodHasEnded env proposalId
  = maybe False (ProposalState.votingPeriodHasEnded env)
  . lookupProposal proposalId

votingPeriodHasNotEnded
  :: ( TracksSlotTime env
     , Proposal p
     )
  => env
  -> Id p
  -> ProposalsState p -> Bool
votingPeriodHasNotEnded env proposalId ps
  = not $ votingPeriodHasEnded env proposalId ps

decision
  :: Proposal p
  => Id p
  -> ProposalsState p
  -> Decision
decision proposalId
  = maybe err ProposalState.decision
  . lookupProposal proposalId
  where
    err = error $ "No proposal with the given hash found."

-- | Remove the approved proposals
--
-- TODO: this is going to mess up 'isApproved' and 'isStablyApproved' So these
-- functions should be placed in the 'ActivationState'
removeApproved :: ProposalsState p -> ([p], ProposalsState p)
removeApproved ProposalsState { proposalStateMap } =
  (approveproposalIdesAndProposals, ProposalsState notApproved)
  where
    (approved, notApproved)    =
      Map.partition (ProposalState.is Approved) proposalStateMap
    approveproposalIdesAndProposals =
      fmap ProposalState.proposal $ Map.elems approved

--------------------------------------------------------------------------------
-- Proposal state query operations
--------------------------------------------------------------------------------

isRevealed
  :: Proposal p
  => Id p -> ProposalsState p -> Bool
isRevealed proposalId = isJust . lookupProposal proposalId

isStablyRevealed
  :: ( TracksSlotTime e
     , Proposal p
     )
  => e -> Id p -> ProposalsState p -> Bool
isStablyRevealed e proposalId
  = maybe False (isStable e . ProposalState.revealedOn)
  . lookupProposal proposalId

is
  :: Proposal p
  => Id p -> Decision -> ProposalsState p -> Bool
is proposalId d
  = maybe False (ProposalState.is d)
  . lookupProposal proposalId

isStably
  :: ( TracksSlotTime e
     , Proposal p
     )
  => e -> Id p -> Decision -> ProposalsState p -> Bool
isStably e proposalId d
  = maybe False (ProposalState.isStably e d)
  . lookupProposal proposalId
