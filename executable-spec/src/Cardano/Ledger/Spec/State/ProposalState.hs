{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Information about the state of a proposal of the update-system
-- (improvement, implementation, etc).
--
module Cardano.Ledger.Spec.State.ProposalState
  ( ProposalState
  , proposal
  , revealedOn
  , decision
  , tally
  , HasVotingPeriod
  , getVotingPeriodDuration
  , newProposalState
  , updateBallot
  , updateBallot'
  , IsVote
  , getVoter
  , getConfidence
  , votingPeriodHasStarted
  , votingPeriodHasEnded
  , votingPeriodEnd
  , Decision (Rejected, WithNoQuorum, Expired, Approved, Undecided)
  , VotingPeriod (VotingPeriod)
  , unVotingPeriod
  -- * Proposal state query operations
  , is
  , isStably
  )
where

import           Cardano.Prelude (NoUnexpectedThunks)

import           Control.Applicative ((<|>))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Word (Word8)
import           GHC.Generics (Generic)

import           Ledger.Core (Slot, SlotCount, (+.))

import           Cardano.Ledger.Spec.Classes.HasAdversarialStakeRatio
                     (HasAdversarialStakeRatio, adversarialStakeRatio)
import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (VKey)
import           Cardano.Ledger.Spec.Classes.HasStakeDistribution
                     (HasStakeDistribution, stakeDistribution)
import           Cardano.Ledger.Spec.Classes.TracksSlotTime (TracksSlotTime,
                     currentSlot, isStable, stableAfter)
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution,
                     stakeOfKeys, totalStake)
import           Cardano.Ledger.Spec.STS.Update.Data
                     (Confidence (Abstain, Against, For))
import           Cardano.Ledger.Spec.STS.Update.Definitions (stakeThreshold)


data ProposalState p d =
  ProposalState
  { proposal             :: !d
  , revealedOn           :: !Slot
    -- ^ Proposals enter the system when they are revealed.
  , votingPeriod         :: !VotingPeriod
    -- ^ Voting period number. To calculate the end of voting period we take the use the formula:
    --
    -- > revealedOn + votingPeriod * (2k + votingPeriodDuration)
    --
  , maxVotingPeriods     :: !VotingPeriod
  , ballot               :: !(Map (Hash p (VKey p)) Confidence)
    -- ^ Votes cast for this proposal.
  , decisionInfo         :: !DecisionInfo
    -- ^ Decision on the proposal. Before the voting period this is set to
    -- 'Undecided'.
  }
  deriving (Generic)

deriving instance (Hashable p, Eq d) => Eq (ProposalState p d)
deriving instance (Hashable p, Show d) => Show (ProposalState p d)

instance ( NoUnexpectedThunks (Hash p (VKey p))
         , NoUnexpectedThunks d
         ) => NoUnexpectedThunks (ProposalState p d)

-- | A voting period number.
newtype VotingPeriod = VotingPeriod { unVotingPeriod :: Word8 }
  deriving stock    (Eq, Show, Generic)
  deriving newtype  (Num, Ord)
  deriving anyclass (NoUnexpectedThunks)

data DecisionInfo =
  DecisionInfo
  { reachedOn   :: !Slot
  , decisionWas :: !Decision
  } deriving (Eq, Show, Generic, NoUnexpectedThunks)

data Decision
  = Rejected
  | WithNoQuorum
  -- ^ The proposal failed to meet quorum (i.e. a majority of the stakeholders
  -- voted "abstain")
  --
  | Expired
  | Approved
  | Undecided
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

class HasVotingPeriod d where
  getVotingPeriodDuration :: d -> SlotCount

instance HasVotingPeriod d => HasVotingPeriod (ProposalState p d) where
  getVotingPeriodDuration = getVotingPeriodDuration . proposal

-- | Tally the votes if the end of the voting period is stable. After tallying
-- the decision state will be changed according to the result of the tallying
-- process. If a the voting period can be extended, then the voting period
-- counter is increased by one.
--
tally
  :: ( Hashable p
     , HasVotingPeriod d
     , TracksSlotTime env
     , HasStakeDistribution t env p
     , HasAdversarialStakeRatio env
     )
  => env
  -> t
  -> ProposalState p d
  -> ProposalState p d
tally env t ps@ProposalState{ votingPeriod, maxVotingPeriods, ballot} =
  if is Undecided ps  && isStable env (votingPeriodEnd env ps)
    then ps { decisionInfo = DecisionInfo
                             { reachedOn   = currentSlot env
                             , decisionWas = tallyResult
                             }
            , votingPeriod = if tallyResult /= Expired
                                then votingPeriod + 1
                                else votingPeriod
            , ballot        = mempty -- The votes get cleaned after a voting period ends.
            }
    else
      ps -- End of the voting period is not stable yet. Nothing to do.
  where
    tallyResult
      =   fromMaybe expiredOrUndecided
      $   tallyStake For     Approved     ballot (stakeDistribution t env) (adversarialStakeRatio env)
      <|> tallyStake Against Rejected     ballot (stakeDistribution t env) (adversarialStakeRatio env)
      <|> tallyStake Abstain WithNoQuorum ballot (stakeDistribution t env) (adversarialStakeRatio env)
      where
        expiredOrUndecided =
          if maxVotingPeriods <= votingPeriod
          then Expired
          else Undecided

tallyStake
  :: Hashable p
  => Confidence
  -> Decision
  -> Map (Hash p (VKey p)) Confidence
  -> StakeDistribution p
  -> Float
  -> Maybe Decision
tallyStake confidence result ballot aStakeDistribution anAdversarialStakeRatio =
  if stakeThreshold anAdversarialStakeRatio (totalStake aStakeDistribution)
     <
     stakeOfKeys votingKeys aStakeDistribution
  then Just result
  else Nothing
  where
    votingKeys = Map.filter (== confidence) ballot

newProposalState
  :: Ord (Hash p (VKey p))
  => Slot
  -> VotingPeriod
  -> d
  -> ProposalState p d
newProposalState theCurrentSlot dMaxVotingPeriods d =
  ProposalState
  { proposal             = d
  , revealedOn           = theCurrentSlot
  , votingPeriod         = 1
  , maxVotingPeriods     = dMaxVotingPeriods
  , ballot               = mempty
  , decisionInfo         = DecisionInfo
                           { reachedOn   = theCurrentSlot
                           , decisionWas = Undecided
                           }
  }

updateBallot
  :: ( Hashable p
     , IsVote p v
     , HasHash p (VKey p)
     )
  => v
  -> ProposalState p d
  -> ProposalState p d
updateBallot v ps =
  updateBallot' (hash $ getVoter v) (getConfidence v) ps

updateBallot'
  :: ( Hashable p
     )
  => Hash p (VKey p)
  -> Confidence
  -> ProposalState p d
  -> ProposalState p d
updateBallot' voterKeyHash confidence ps =
  ps { ballot = Map.insert voterKeyHash confidence (ballot ps) }


class IsVote p v | v -> p where
  getVoter :: v -> VKey p

  getConfidence :: v -> Confidence

votingPeriodHasStarted
  :: TracksSlotTime env
  => env
  -> ProposalState p d
  -> Bool
votingPeriodHasStarted env ProposalState { revealedOn } = isStable env revealedOn

votingPeriodHasEnded
  :: ( HasVotingPeriod d
     , TracksSlotTime env
     )
  => env
  -> ProposalState p d
  -> Bool
votingPeriodHasEnded env ps = votingPeriodEnd env ps <= currentSlot env

-- | Calculate the slot at which the voting period ends.
--
-- The voting period starts @2k@ slots after the revelation, and lasts for
-- 'votingPeriodDuration' slots. When tallying, a voting period can be extended.
-- Tallying takes place @2k@ slots after the end of the voting period.
--
--
-- In general the end of the i-th voting period is calculated as:
--
-- > revealedOn + i * (2k + votingPeriodDuration)
--
-- To see why we use this formula consider the following example: if we run 3
-- voting periods we have the following time-line:
--
-- - revelation slot
-- ... [ 2k slots ]
-- - voting period 1 starts
-- ... [ voting period duration ]
-- - voting period 1 ends
-- ... [ 2k slots ]
-- - tallying / voting period 2 starts
-- ... [ voting period duration]
-- - voting period 2 ends
-- ... [ 2k slots ]
-- - tallying / voting period 2 starts
-- ... [ voting period duration ]
-- - voting period 3 ends
--
-- So in general it takes @2k@ slots till the voting period starts: the first
-- voting period requires the stabilization of the revelation, and the
-- subsequent voting periods require the stabilization of the precedent voting
-- period. After the voting period starts it lasts for @votingPeriodDuration@.
-- So one voting period lasts for @2k + votingPeriodDuration@.
--
-- TODO: we might want to change `Word` to `Natural`, or check for overflows.
votingPeriodEnd
  :: ( HasVotingPeriod d
     , TracksSlotTime env
     )
  => env
  -> ProposalState p d
  -> Slot
votingPeriodEnd env ps@ProposalState { revealedOn, votingPeriod }
  =  revealedOn
  +. votingPeriod' * ( stableAfter env + votingPeriodDuration)
  where
    votingPeriod'        = fromIntegral (unVotingPeriod votingPeriod)
    votingPeriodDuration = getVotingPeriodDuration ps

--------------------------------------------------------------------------------
-- State query operations
--------------------------------------------------------------------------------

is :: Decision -> ProposalState p d -> Bool
is d = (== d) . decision

decision :: ProposalState p d -> Decision
decision = decisionWas . decisionInfo

isStably
  :: TracksSlotTime e
  => e
  -> Decision
  -> ProposalState p d
  -> Bool
isStably e d st
  =  (isStable e . reachedOn . decisionInfo) st
  && is d st

--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
