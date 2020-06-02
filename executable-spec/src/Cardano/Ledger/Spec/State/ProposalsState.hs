{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Information about different proposals in the update system.
--
module Cardano.Ledger.Spec.State.ProposalsState
  ( ProposalsState
  , tally
  , revealProposal
  , updateBallot
  , updateBallot'
  , votingPeriodStarted
  , votingPeriodEnded
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
  )
where

import           Cardano.Prelude (NoUnexpectedThunks)

import           Control.Arrow (second)
import           Control.Exception (assert)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           GHC.Generics (Generic)

import           Ledger.Core (BlockCount, Slot)

import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (VKey)
import           Cardano.Ledger.Spec.Classes.TracksSlotTime (TracksSlotTime,
                     isStable)
import           Cardano.Ledger.Spec.State.ProposalState (Decision (Approved),
                     HasVotingPeriod, IsVote, ProposalState, VotingPeriod,
                     newProposalState)
import qualified Cardano.Ledger.Spec.State.ProposalState as ProposalState
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import           Cardano.Ledger.Spec.STS.Update.Data (Confidence)

-- | The @d@ parameter is the type of the proposals. The @p@ parameter
-- determines the hashing and cryptographic algorithms to be used in the
-- proposal state.
newtype ProposalsState p d =
  ProposalsState { proposalStateMap :: (Map (Hash p d) (ProposalState p d)) }
  deriving (Eq, Show, Generic)

deriving instance Ord (Hash p d) => Semigroup (ProposalsState p d)
deriving instance Ord (Hash p d) => Monoid (ProposalsState p d)
instance ( NoUnexpectedThunks (Hash p d)
         , NoUnexpectedThunks (Hash p (VKey p))
         , NoUnexpectedThunks d
         ) => NoUnexpectedThunks (ProposalsState p d)

lookupProposal
  :: Hashable p
  => Hash p d -> ProposalsState p d -> Maybe (ProposalState p d)
lookupProposal dHash = Map.lookup dHash . proposalStateMap

-- | Tally each proposal in the state.
--
tally
  :: ( Hashable p
     , HasVotingPeriod d
     )
  => BlockCount
  -- ^ Chain stability parameter.
  -> Slot
  -> StakeDistribution p
  -> Float
  -- ^ Adversarial stake ratio.
  -> ProposalsState p d
  -> ProposalsState p d
tally k
      currentSlot
      stakeDistribution
      adversarialStakeRatio
      ProposalsState { proposalStateMap }
  = ProposalsState
  $ fmap (ProposalState.tally k currentSlot stakeDistribution adversarialStakeRatio)
         proposalStateMap

-- | Register the revelation of a proposal.
--
revealProposal
  :: ( Hashable p
     , HasHash p d
     )
  => Slot
  -> VotingPeriod
  -> d
  -- ^ Proposal being revealed.
  -> ProposalsState p d
  -> ProposalsState p d
revealProposal currentSlot dMaxVotingPeriods d ProposalsState { proposalStateMap }
  = ProposalsState
  $ Map.insert (hash d) (newProposalState currentSlot dMaxVotingPeriods d) proposalStateMap

-- | Is the proposal revealed.
isRevealed'
  :: ( Hashable p
     , HasHash p d
     )
  => d
  -> ProposalsState p d
  -> Bool
isRevealed' d ProposalsState { proposalStateMap }
  = Map.member (hash d) proposalStateMap

isNotRevealed'
  :: ( Hashable p
     , HasHash p d
     )
  => d
  -> ProposalsState p d
  -> Bool
isNotRevealed' d st = not $ isRevealed' d st

-- | Register the vote for the given proposal's hash.
--
-- The proposal must be present in the map, otherwise an 'AssertionFailed'
-- exception will be thrown, unless the code is compiles with assertions
-- disabled. See 'Control.Exception.assert'.
--
-- TODO: we should remove this in favor of 'updateBallot''.
updateBallot
  :: ( Hashable p
     , IsVote p v
     , HasHash p (VKey p)
     )
  => Hash p d
  -> v
  -> ProposalsState p d
  -> ProposalsState p d
updateBallot dHash v ProposalsState { proposalStateMap }
  =  assert (dHash `Map.member` proposalStateMap)
  $  ProposalsState
  $! Map.adjust (ProposalState.updateBallot v) dHash proposalStateMap

updateBallot'
  :: Hashable p
  => Hash p d
  -> Hash p (VKey p)
  -> Confidence
  -> ProposalsState p d
  -> ProposalsState p d
updateBallot' dHash voterKeyHash confidence ProposalsState { proposalStateMap }
  =  assert (dHash `Map.member` proposalStateMap)
  $  ProposalsState
  $! Map.adjust (ProposalState.updateBallot' voterKeyHash confidence) dHash proposalStateMap

votingPeriodStarted
  :: Hashable p
  => BlockCount
  -> Slot
  -> Hash p d
  -> ProposalsState p d
  -> Bool
votingPeriodStarted k currentSlot dHash
  = maybe False (ProposalState.votingPeriodStarted k currentSlot)
  . lookupProposal dHash

votingPeriodEnded
  :: ( Hashable p
     , HasVotingPeriod d
     )
  => BlockCount
  -> Slot
  -> Hash p d
  -> ProposalsState p d -> Bool
votingPeriodEnded k currentSlot dHash
  = maybe False (ProposalState.votingPeriodEnded k currentSlot)
  . lookupProposal dHash

votingPeriodHasNotEnded
  :: ( Hashable p
     , HasVotingPeriod d
     )
  => BlockCount
  -> Slot
  -> Hash p d
  -> ProposalsState p d -> Bool
votingPeriodHasNotEnded k currentSlot dHash ps
  = not $ votingPeriodEnded k currentSlot dHash ps

decision
  :: Hashable p
  => Hash p d
  -> ProposalsState p d
  -> Decision
decision dHash
  = maybe err ProposalState.decision
  . lookupProposal dHash
  where
    err = error $ "No proposal with the given hash found."

-- | Remove the approved proposals
--
-- TODO: this is going to mess up 'isApproved' and 'isStablyApproved' So these
-- functions should be placed in the 'ActivationState'
removeApproved :: ProposalsState p d -> ([(Hash p d, d)], ProposalsState p d)
removeApproved ProposalsState { proposalStateMap } =
  (approvedHashesAndProposals, ProposalsState notApproved)
  where
    (approved, notApproved)    =
      Map.partition (ProposalState.is Approved) proposalStateMap
    approvedHashesAndProposals =
      fmap (second ProposalState.proposal) $ Map.toList approved

--------------------------------------------------------------------------------
-- Proposal state query operations
--------------------------------------------------------------------------------

isRevealed
  :: Hashable p
  => Hash p d -> ProposalsState p d -> Bool
isRevealed dHash = isJust . lookupProposal dHash

isStablyRevealed
  :: ( Hashable p
     , TracksSlotTime e
     )
  => e -> Hash p d -> ProposalsState p d -> Bool
isStablyRevealed e dHash
  = maybe False (isStable e . ProposalState.revealedOn)
  . lookupProposal dHash

is
  :: Hashable p
  => Hash p d -> Decision -> ProposalsState p d -> Bool
is dHash d
  = maybe False (ProposalState.is d)
  . lookupProposal dHash

isStably
  :: ( Hashable p
     , TracksSlotTime e
     )
  => e -> Hash p d -> Decision -> ProposalsState p d -> Bool
isStably e dHash d
  = maybe False (ProposalState.isStably e d)
  . lookupProposal dHash
