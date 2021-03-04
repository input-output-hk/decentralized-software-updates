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
module Cardano.Ledger.Update.ProposalState
  ( ProposalState
  , proposal
  , revealedOn
  , decision
  , tally
  , newProposalState
  , updateBallot
  , updateBallot'
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

import           Control.Applicative ((<|>))
import           Control.DeepSeq (NFData)
import           Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR),
                     decodeInt, decodeListLenOf, encodeInt, encodeListLen)
import           Cardano.Slotting.Slot (SlotNo)

import           Cardano.Ledger.Update.Env.HasAdversarialStakeRatio
                     (HasAdversarialStakeRatio, adversarialStakeRatio)
import           Cardano.Ledger.Update.Env.HasStakeDistribution
                     (HasStakeDistribution, stakeDistribution)
import           Cardano.Ledger.Update.Env.HasVotingPeriodsCap
                     (VotingPeriod (VotingPeriod, unVotingPeriod))
import           Cardano.Ledger.Update.Env.StakeDistribution (StakeDistribution,
                     stakeOfKeys, stakeThreshold, totalStake)
import           Cardano.Ledger.Update.Env.TracksSlotTime (TracksSlotTime,
                     currentSlot, isStable, stableAfter)

-- TODO: import explicitly
import           Cardano.Ledger.Update.Proposal hiding (confidence, proposal)
import qualified Cardano.Ledger.Update.Proposal as Proposal

data ProposalState p =
  ProposalState
  { proposal             :: !p
  , revealedOn           :: !SlotNo
    -- ^ Proposals enter the system when they are revealed.
  , votingPeriod         :: !VotingPeriod
    -- ^ Voting period number. To calculate the end of voting period we take the use the formula:
    --
    -- > revealedOn + votingPeriod * (stableAfter + votingPeriodDuration)
    --
  , maxVotingPeriods     :: !VotingPeriod
  , ballot               :: !(Map (Id (Voter p)) Confidence)
    -- ^ Votes cast for this proposal.
  , decisionInfo         :: !DecisionInfo
    -- ^ Decision on the proposal. Before the voting period this is set to
    -- 'Undecided'.
  }
  deriving (Generic)

deriving instance (Proposal p) =>
  Show (ProposalState p)

deriving instance (Eq p, Proposal p) =>
  Eq (ProposalState p)

deriving instance (NFData p, NFData (Id (Voter p))) =>
  NFData (ProposalState p)

deriving instance (ToJSON p, ToJSONKey (Id (Voter p))) =>
  ToJSON (ProposalState p)

deriving instance (FromJSON p, FromJSONKey (Id (Voter p)), Proposal p) =>
  FromJSON (ProposalState p)

instance ( NoThunks p, NoThunks (Voter p), NoThunks (Id (Voter p))) =>
  NoThunks (ProposalState p)

data DecisionInfo =
  DecisionInfo
  { reachedOn   :: !SlotNo
  , decisionWas :: !Decision
  } deriving (Eq, Show, Generic, NoThunks, NFData, ToJSON, FromJSON)

instance ToCBOR DecisionInfo where
  toCBOR di
    =  encodeListLen 2
    <> toCBOR (reachedOn di)
    <> toCBOR (decisionWas di)

instance FromCBOR DecisionInfo where
  fromCBOR = do
    decodeListLenOf 2
    ro <- fromCBOR
    dw <- fromCBOR
    return $! DecisionInfo ro dw

data Decision
  = Rejected
  | WithNoQuorum
  -- ^ The proposal failed to meet quorum (i.e. a majority of the stakeholders
  -- voted "abstain")
  --
  | Expired
  | Approved
  | Undecided
  deriving (Eq, Ord, Enum, Bounded, Show, Generic, NoThunks, NFData, ToJSON, FromJSON)

decisionEncoding :: IntMap Decision
decisionEncoding = IntMap.fromList [ (0, Rejected)
                                   , (1, WithNoQuorum)
                                   , (2, Expired)
                                   , (3, Approved)
                                   , (4, Undecided)
                                   ]

instance ToCBOR Decision where
  toCBOR = encodeInt . fromEnum

instance FromCBOR Decision where
  fromCBOR = do
    i <- decodeInt
    case IntMap.lookup i decisionEncoding of
      Just k  -> return $! k
      Nothing -> fail $  "Decoded integer value '" <> show i
                      <> "' is an invalid encoding of a value of type 'Decision'"

-- | Tally the votes if the end of the voting period is stable. After tallying
-- the decision state will be changed according to the result of the tallying
-- process. If a the voting period can be extended, then the voting period
-- counter is increased by one.
--
tally
  :: ( TracksSlotTime env
     , HasStakeDistribution env (Id (Voter p))
     , HasAdversarialStakeRatio env
     , Proposal p
     )
  => env
  -> ProposalState p
  -> ProposalState p
tally env ps@ProposalState{ votingPeriod, maxVotingPeriods, ballot} =
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
      $   tallyStake For     Approved     ballot (stakeDistribution env) (adversarialStakeRatio env)
      <|> tallyStake Against Rejected     ballot (stakeDistribution env) (adversarialStakeRatio env)
      <|> tallyStake Abstain WithNoQuorum ballot (stakeDistribution env) (adversarialStakeRatio env)
      where
        expiredOrUndecided =
          if maxVotingPeriods <= votingPeriod
          then Expired
          else Undecided

tallyStake
  :: Proposal p
  => Confidence
  -> Decision
  -> Map (Id (Voter p)) Confidence
  -> StakeDistribution (Id (Voter p))
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
  :: Proposal p
  => SlotNo
  -> VotingPeriod
  -> p
  -> ProposalState p
newProposalState theCurrentSlot dMaxVotingPeriods p =
  ProposalState
  { proposal             = p
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
  :: Proposal p
  => (Vote p)
  -> ProposalState p
  -> ProposalState p
updateBallot v ps =
  updateBallot' (voter v) (Proposal.confidence v) ps

updateBallot'
  :: Proposal p
  => Id (Voter p)
  -> Confidence
  -> ProposalState p
  -> ProposalState p
updateBallot' voterId confidence ps =
  ps { ballot = Map.insert voterId confidence (ballot ps) }

votingPeriodHasStarted
  :: TracksSlotTime env
  => env
  -> ProposalState p
  -> Bool
votingPeriodHasStarted env ProposalState { revealedOn } =
  isStable env revealedOn

votingPeriodHasEnded
  :: ( TracksSlotTime env
     , Proposal p
     )
  => env
  -> ProposalState p
  -> Bool
votingPeriodHasEnded env ps
  =  votingPeriodEnd env ps <= currentSlot env
  || decisionWas (decisionInfo ps) /= Undecided

-- | Calculate the slot at which the voting period ends.
--
-- The voting period starts @stableAfter@ slots after the revelation, and lasts for
-- 'votingPeriodDuration' slots. When tallying, a voting period can be extended.
-- Tallying takes place @stableAfter@ slots after the end of the voting period.
--
--
-- In general the end of the i-th voting period is calculated as:
--
-- > revealedOn + i * (stableAfter + votingPeriodDuration)
--
-- To see why we use this formula consider the following example: if we run 3
-- voting periods we have the following time-line:
--
-- - revelation slot
-- ... [ stableAfter slots have passed ]
-- - voting period 1 starts
-- ... [ voting period duration ]
-- - voting period 1 ends
-- ... [ stableAfter slots have passed ]
-- - tallying / voting period 2 starts
-- ... [ voting period duration]
-- - voting period 2 ends
-- ... [ stableAfter slots have passed ]
-- - tallying / voting period 2 starts
-- ... [ voting period duration ]
-- - voting period 3 ends
--
-- So in general it takes @stableAfter@ slots till the voting period starts: the first
-- voting period requires the stabilization of the revelation, and the
-- subsequent voting periods require the stabilization of the precedent voting
-- period. After the voting period starts it lasts for @votingPeriodDuration@.
-- So one voting period lasts for @stableAfter + votingPeriodDuration@.
--
votingPeriodEnd
  :: (Proposal p, TracksSlotTime env)
  => env
  -> ProposalState p
  -> SlotNo
votingPeriodEnd env ProposalState { proposal, revealedOn, votingPeriod }
  = revealedOn
  + votingPeriod' * ( stableAfter env + votingPeriodDuration proposal)
  where
    votingPeriod'        = fromIntegral (unVotingPeriod votingPeriod)

--------------------------------------------------------------------------------
-- State query operations
--------------------------------------------------------------------------------

is :: Decision -> ProposalState p -> Bool
is d = (== d) . decision

decision :: ProposalState p -> Decision
decision = decisionWas . decisionInfo

isStably
  :: TracksSlotTime e
  => e
  -> Decision
  -> ProposalState p
  -> Bool
isStably e d st
  =  (isStable e . reachedOn . decisionInfo) st
  && is d st

--------------------------------------------------------------------------------
-- To/FromCBOR instances
--------------------------------------------------------------------------------

instance (Typeable p, ToCBOR p, Proposal p, ToCBOR (Id (Voter p))) =>
  ToCBOR (ProposalState p) where
  toCBOR ps
    =  encodeListLen 6
    <> toCBOR (proposal ps)
    <> toCBOR (revealedOn ps)
    <> toCBOR (votingPeriod ps)
    <> toCBOR (maxVotingPeriods ps)
    <> toCBOR (ballot ps)
    <> toCBOR (decisionInfo ps)

instance (Typeable p, FromCBOR p, Proposal p, FromCBOR (Id (Voter p))) =>
  FromCBOR (ProposalState p) where
  fromCBOR = do
    decodeListLenOf 6
    pr <- fromCBOR
    ro <- fromCBOR
    vp <- fromCBOR
    mp <- fromCBOR
    ba <- fromCBOR
    di <- fromCBOR
    return $! ProposalState pr ro vp mp ba di
