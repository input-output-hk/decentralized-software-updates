{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Information about different proposals in the update system.
--
module Cardano.Ledger.Spec.State.ProposalsState
  ( ProposalsState (..)
  , tally
  , revealProposal
  , updateBallot
  , votingPeriodStarted
  , votingPeriodEnded
  , votingPeriodHasNotEnded
  , decision
  , isRevealed
  , isNotRevealed
  )
where

import           Control.Exception (assert)
import qualified Data.Map as Map
import           Data.Map.Strict (Map)

import           Ledger.Core (BlockCount, Slot)

import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (VKey)
import           Cardano.Ledger.Spec.State.ProposalState (Decision,
                     HasVotingPeriod, IsVote, ProposalState, VotingPeriod,
                     newProposalState)
import qualified Cardano.Ledger.Spec.State.ProposalState as ProposalState
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import           Cardano.Ledger.Spec.STS.Update.Data (Stake)

newtype ProposalsState p d = ProposalsState (Map (Hash p d) (ProposalState p))
  deriving (Eq, Show)

deriving instance Ord (Hash p d) => Semigroup (ProposalsState p d)
deriving instance Ord (Hash p d) => Monoid (ProposalsState p d)

-- | Tally each proposal in the state.
--
tally
  :: (Ord (Hash p (VKey p )))
  => BlockCount
  -- ^ Chain stability parameter.
  -> Slot
  -> (StakeDistribution p, Stake)
  -> Float
  -- ^ Adversarial stake ratio.
  -> ProposalsState p d
  -> ProposalsState p d
tally k
      currentSlot
      stakeDistribution
      adversarialStakeRatio
      (ProposalsState proposalStateMap)
  = ProposalsState
  $ fmap (ProposalState.tally k currentSlot stakeDistribution adversarialStakeRatio)
         proposalStateMap

-- | Register the revelation of a proposal.
--
revealProposal
  :: ( Hashable p
     , HasHash p d
     , HasVotingPeriod d
     )
  => Slot
  -> VotingPeriod
  -> d
  -> ProposalsState p d
  -> ProposalsState p d
revealProposal currentSlot dMaxVotingPeriods d (ProposalsState proposalStateMap)
  = ProposalsState
  $ Map.insert (hash d) (newProposalState currentSlot dMaxVotingPeriods d) proposalStateMap

-- | Is the proposal revealed.
isRevealed
  :: ( Hashable p
     , HasHash p d
     )
  => d
  -> ProposalsState p d
  -> Bool
isRevealed d (ProposalsState proposalStateMap)
  = Map.member (hash d) proposalStateMap

isNotRevealed
  :: ( Hashable p
     , HasHash p d
     )
  => d
  -> ProposalsState p d
  -> Bool
isNotRevealed d st = not $ isRevealed d st

-- | Register the vote for the given proposal's hash.
--
-- The proposal must be present in the map, otherwise an 'AssertionFailed'
-- exception will be thrown, unless the code is compiles with assertions
-- disabled. See 'Control.Exception.assert'.
updateBallot
  :: ( Hashable p
     , IsVote p v
     , HasHash p (VKey p)
     )
  => Hash p d
  -> v
  -> ProposalsState p d
  -> ProposalsState p d
updateBallot dHash v (ProposalsState proposalStateMap)
  = assert (dHash `Map.member` proposalStateMap)
  $ ProposalsState
  $ Map.adjust (ProposalState.updateBallot v) dHash proposalStateMap

votingPeriodStarted
  :: (Ord (Hash p d))
  => BlockCount
  -> Slot
  -> Hash p d
  -> ProposalsState p d
  -> Bool
votingPeriodStarted k currentSlot dHash (ProposalsState proposalStateMap)
  = maybe False (ProposalState.votingPeriodStarted k currentSlot)
  $ Map.lookup dHash proposalStateMap

votingPeriodEnded
  :: (Ord (Hash p d))
  => BlockCount
  -> Slot
  -> Hash p d
  -> ProposalsState p d -> Bool
votingPeriodEnded k currentSlot dHash (ProposalsState proposalStateMap)
  = maybe False (ProposalState.votingPeriodEnded k currentSlot)
  $ Map.lookup dHash proposalStateMap

votingPeriodHasNotEnded
  :: (Ord (Hash p d))
  => BlockCount
  -> Slot
  -> Hash p d
  -> ProposalsState p d -> Bool
votingPeriodHasNotEnded k currentSlot dHash ps
  = not $ votingPeriodEnded k currentSlot dHash ps

decision
  :: (Ord (Hash p d))
  => Hash p d
  -> ProposalsState p d
  -> Decision
decision dHash (ProposalsState proposalStateMap)
  = maybe err ProposalState.decision
  $ Map.lookup dHash proposalStateMap
  where
    err = error $ "No proposal with the given hash found."
