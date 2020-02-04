{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Information about the state of a proposal of the update-system
-- (improvement, implementation, etc).
--
module Cardano.Ledger.Spec.State.ProposalState
  ( ProposalState (decision)
  , tally
  , HasVotingPeriod
  , getVotingPeriodDuration
  , newProposalState
  , updateBallot
  , IsVote
  , getVoter
  , getConfidence
  , votingPeriodStarted
  , votingPeriodEnded
  , votingPeriodEnd
  , Decision (Rejected, NoQuorum, Expired, Accepted, Undecided)
  , VotingPeriod (VotingPeriod)
  )
where

import           Control.Applicative ((<|>))
import           Data.Coerce (coerce)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Word (Word8)
import           GHC.Generics (Generic)

import           Ledger.Core (BlockCount (BlockCount), Slot,
                     SlotCount (SlotCount), (*.), (+.))

import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (VKey)
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution,
                     stakePercentOfKeys)
import           Cardano.Ledger.Spec.STS.Update.Data
                     (Confidence (Abstain, Against, For))
import           Cardano.Ledger.Spec.STS.Update.Definitions (vThreshold)

data ProposalState p =
  ProposalState
  { revealedOn           :: !Slot
    -- ^ Proposals enter the system when they are revealed.
  , votingPeriodDuration :: !SlotCount
  , votingPeriod         :: !VotingPeriod
    -- ^ Voting period number. To calculate the end of voting period we take the use the formula:
    --
    -- > revealedOn + votingPeriod * (2k + votingPeriodDuration)
    --
  , maxVotingPeriods     :: !VotingPeriod
  , ballot               :: Map (Hash p (VKey p)) Confidence
    -- ^ Votes cast for this proposal.
  , decision             :: Decision
    -- ^ Decision on the proposal. Before the voting period this is set to
    -- 'Undecided'.
  }
  deriving (Generic)

deriving instance Hashable p => Eq (ProposalState p)
deriving instance Hashable p => Show (ProposalState p)

-- | A voting period number.
newtype VotingPeriod = VotingPeriod { unVotingPeriod :: Word8 }
  deriving (Eq, Show, Generic, Num, Ord)

data Decision = Rejected | NoQuorum | Expired | Accepted | Undecided
  deriving (Eq, Show, Generic)

class HasVotingPeriod d where
  getVotingPeriodDuration :: d -> SlotCount

-- | Tally the votes if the end of the voting period is stable. After tallying
-- the decision state will be changed according to the result of the tallying
-- process. If a the voting period can be extended, then the voting period
-- counter is increased by one.
--
tally
  :: Ord (Hash p (VKey p))
  => BlockCount
  -> Slot
  -> StakeDistribution p
  -> Float
  -> ProposalState p
  -> ProposalState p
tally k
      currentSlot
      stakeDistribution
      adversarialStakeRatio
      (ps@ProposalState
        { votingPeriod
        , maxVotingPeriods
        , ballot
        , decision
        })
  =
  if decision == Undecided && votingPeriodEnd k ps +. 2 *. k  <= currentSlot
    then ps { decision     = tallyResult

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
      $   tallyStake For     Accepted ballot stakeDistribution adversarialStakeRatio
      <|> tallyStake Against Rejected ballot stakeDistribution adversarialStakeRatio
      <|> tallyStake Abstain NoQuorum ballot stakeDistribution adversarialStakeRatio
      where
        expiredOrUndecided =
          if maxVotingPeriods <= votingPeriod
          then Expired
          else Undecided

tallyStake
  :: Ord (Hash p (VKey p))
  => Confidence
  -> Decision
  -> Map (Hash p (VKey p)) Confidence
  -> StakeDistribution p
  -> Float
  -> Maybe Decision
tallyStake confidence result ballot stakeDistribution adversarialStakeRatio =
  if vThreshold adversarialStakeRatio < stakePercentOfKeys votingKeys stakeDistribution
  then Just result
  else Nothing
  where
    votingKeys = Map.keys $ Map.filter (== confidence) ballot

newProposalState
  :: (Ord (Hash p (VKey p)), HasVotingPeriod d)
  => Slot
  -> VotingPeriod
  -> d
  -> ProposalState p
newProposalState currentSlot dMaxVotingPeriods d =
  ProposalState
  { revealedOn = currentSlot
  , votingPeriodDuration = getVotingPeriodDuration d
  , votingPeriod = 1
  , maxVotingPeriods = dMaxVotingPeriods
  , ballot = mempty
  , decision = Undecided
  }

updateBallot
  :: ( Hashable p
     , IsVote p v
     , HasHash p (VKey p)
     )
  => v
  -> ProposalState p
  -> ProposalState p
updateBallot v ps =
  ps { ballot = Map.insert (hash $ getVoter v) (getConfidence v) (ballot ps) }

class IsVote p v | v -> p where
  getVoter :: v -> VKey p

  getConfidence :: v -> Confidence

votingPeriodStarted
  :: BlockCount
  -> Slot
  -> ProposalState p
  -> Bool
votingPeriodStarted
  k
  currentSlot
  ProposalState { revealedOn }
  =
  votingPeriodStart <= currentSlot
  where
    votingPeriodStart = revealedOn +. 2 * (coerce k)

votingPeriodEnded
  :: BlockCount
  -> Slot
  -> ProposalState p
  -> Bool
votingPeriodEnded k currentSlot ps = votingPeriodEnd k ps <= currentSlot

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
  :: BlockCount
  -> ProposalState p
  -> Slot
votingPeriodEnd k ProposalState { revealedOn, votingPeriod, votingPeriodDuration }
  =  revealedOn
  +. votingPeriod' * ( 2 * coerce k + votingPeriodDuration)
  where
    votingPeriod' = fromIntegral (unVotingPeriod votingPeriod)
