{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Benchmarks.Update.Tally where

import           NoThunks.Class (NoThunks)

import           Control.Monad ((<$!>))
import           Data.List (repeat, zip)
import           Data.List (foldl')
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Cardano.Binary (ToCBOR, toCBOR)
import qualified Codec.CBOR.Write as CBOR.Write
import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short

import           Cardano.Slotting.Block (BlockNo, unBlockNo)
import           Cardano.Slotting.Slot (SlotNo)

import           Cardano.Ledger.Update.Env.HasAdversarialStakeRatio
                     (HasAdversarialStakeRatio)
import qualified Cardano.Ledger.Update.Env.HasAdversarialStakeRatio
import           Cardano.Ledger.Update.Env.HasStakeDistribution
                     (HasStakeDistribution)
import qualified Cardano.Ledger.Update.Env.HasStakeDistribution
import           Cardano.Ledger.Update.Env.StakeDistribution (StakeDistribution,
                     fromList)
import           Cardano.Ledger.Update.Env.TracksSlotTime (TracksSlotTime,
                     stableAfter)
import qualified Cardano.Ledger.Update.Env.TracksSlotTime
import           Cardano.Ledger.Update.ProposalsState (ProposalsState, decision,
                     initialState, reveal, tally, updateBallot')
import           Cardano.Ledger.Update.ProposalsState (Decision,
                     VotingPeriod (VotingPeriod))

import           Cardano.Ledger.Update.Proposal


data BenchmarkConstants =
  BenchmarkConstants
  { k                :: !BlockNo
  -- ^ Chain stability parameter.
  , r_a              :: !Float
  -- ^ Adversarial stake ratio.
  , revelationSlot   :: !SlotNo
  -- ^ SlotNo at which __all__ proposals were registered. This is used when
  -- revealing a proposal, and to compute the slot a which tally should take
  -- place.
  }
  deriving (Show, Eq)

-- | Data required for performing tallying the votes.
data TallyData =
  TallyData
  { stakeDist          :: !(StakeDistribution (Id (Voter BenchProposal)))
  , proposals          :: ![BenchProposal]
  -- ^ Update proposals that are active at the same time. We assume their voting
  -- period overlaps exactly, which is the worst case.
  , proposalHashes     :: ![Id BenchProposal]
  -- ^ Proposal hashes. These must correspond to 'proposals'. We use this to
  -- avoid computing hashes when getting the results of the tally.
  , participantsHashes :: ![Id (Voter BenchProposal)]
  }
  deriving stock (Show, Generic)
  deriving anyclass (NoThunks)

-- | Simulate the revelation of the number of proposals given by the benchmark
-- parameters.
revealProposals
  :: BenchmarkConstants
  -> TallyData
  -> ProposalsState BenchProposal
revealProposals BenchmarkConstants { revelationSlot } TallyData { proposals } =
  foldl' revealProposal initialState proposals
  where
   -- For the purposes of benchmarking the tally process we only need a single
   -- voting period.
    revealProposal st aProposal =
      reveal revelationSlot (VotingPeriod 1) aProposal st

-- | Vote on all the proposals in the state.
voteOnProposals
  :: TallyData
  -> ProposalsState BenchProposal
  -> ProposalsState BenchProposal
voteOnProposals TallyData {participantsHashes, proposals} st =
  foldl' voteOnProposal st proposalsHashes
  where
    voteOnProposal st' hp   =
      foldl' vote st' participantsHashes
      where
        vote st'' voterKeyHash = updateBallot' hp voterKeyHash For st''
    proposalsHashes = fmap _id proposals

-- | Get number of participants and run the tally
runTally
  :: BenchmarkConstants
  -> (TallyData, ProposalsState BenchProposal)
  -> [Decision]
runTally
  BenchmarkConstants { k, r_a, revelationSlot }
  (TallyData { stakeDist, proposalHashes }, proposalsState)
  = fmap (`decision` proposalsStateAfterTally) proposalHashes
  where
    proposalsStateAfterTally = tally env proposalsState
      where
        env = TallyEnv
              { envK                     = k
              , envCurrentSlot           = currentSlot
              , envStakeDist             = stakeDist
              , envAdversarialStakeRatio = r_a
              }
        -- The current slot should be set to a value that causes tally to take
        -- place without the voting period to become expired.
        --
        -- Here we're assuming a vote period duration of 1, as defined in the
        -- 'HasVotingPeriod' instance of 'Vote'.
        currentSlot = revelationSlot
                    + stableAfter env
                    -- Now revelation is stable.
                    + votingPeriodDuration (undefined :: BenchProposal)
                    -- Now the voting period ended (bench proposals have the
                    -- same voting period duration).
                    + stableAfter env
                    -- Now the end of the voting period is stable: tally can
                    -- take place.

data TallyEnv =
  TallyEnv
  { envK                     :: !BlockNo
  , envCurrentSlot           :: !SlotNo
  , envStakeDist             :: !(StakeDistribution (Id (Voter BenchProposal)))
  , envAdversarialStakeRatio :: !Float
  }

instance TracksSlotTime TallyEnv where
  stableAfter    = (2 *) . fromIntegral . unBlockNo . envK
  currentSlot    = envCurrentSlot
  slotsPerEpoch  = error "Benchmarks should not use this"
  epochFirstSlot = error "Benchmarks should not use this"

instance HasStakeDistribution TallyEnv (Id (Voter BenchProposal)) where
  stakeDistribution = envStakeDist

instance HasAdversarialStakeRatio TallyEnv where
  adversarialStakeRatio = envAdversarialStakeRatio

newtype NumberOfParticipants = NumberOfParticipants Word64

newtype NumberOfConcurrentUPs = NumberOfConcurrentUPs Word64

-- | Create the data used for the benchmarks. It fully evaluates the data.
createTallyData
  :: BenchmarkConstants
  -> NumberOfParticipants
  -> NumberOfConcurrentUPs
  -> (TallyData, ProposalsState BenchProposal)
createTallyData
  constants
  (NumberOfParticipants numOfParticipants)
  (NumberOfConcurrentUPs numOfConcurrentUPs)
  =
  (tallyData, proposalsState)
  where
    !tallyData =
      TallyData
      { stakeDist          = mkStakeDist theParticipantsHashes
      , proposals          = proposals'
      , proposalHashes     = forceList $ _id <$!> proposals'
      , participantsHashes = theParticipantsHashes
      }
    proposals'            = BenchProposal <$!> [1.. numOfConcurrentUPs]
    !proposalsState       =  voteOnProposals tallyData
                          $! revealProposals constants tallyData
    participants          = BenchVoter <$!> [1 .. fromIntegral numOfParticipants]
    theParticipantsHashes = _id <$!> participants


forceList :: [a] -> [a]
forceList xs = forceElements xs `seq` xs

forceElements :: [a] -> ()
forceElements = foldr seq ()

-- | Uniform stake distribution with a stake of 1 for each stakeholder.
mkStakeDist
  :: [Id (Voter BenchProposal)] -> StakeDistribution (Id (Voter BenchProposal))
mkStakeDist participantsHashes = fromList $ zip participantsHashes (repeat 1)

--------------------------------------------------------------------------------
-- Proposals used in the benchmarks
--------------------------------------------------------------------------------

newtype BenchProposal = BenchProposal Word64
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NoThunks)
  deriving newtype (ToCBOR)


instance Identifiable BenchProposal where
  newtype Id BenchProposal = BenchProposalId BenchHash
    deriving stock (Ord, Eq, Show, Generic)
    deriving anyclass (NoThunks)

  _id = BenchProposalId . byronHash

instance Proposal BenchProposal where
  data Submission BenchProposal = BenchSubmission BenchHash
    deriving (Eq, Show)

  data Revelation BenchProposal = BenchRevelation BenchProposal
    deriving (Eq, Show)

  revelationCommit (BenchSubmission benchHash) = benchHash

  proposal (BenchRevelation  benchProposal) = benchProposal

  votingPeriodDuration = const 1

  data Vote BenchProposal =
    BenchVote
    { bvVoterId    :: Id (Voter BenchProposal)
    , bvCandidate  :: Id BenchProposal
    , bvConfidence :: Confidence
    } deriving (Show)

  newtype Voter BenchProposal = BenchVoter Word64
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (ToCBOR)
    deriving anyclass (NoThunks)

  voter      = bvVoterId
  candidate  = bvCandidate
  confidence = bvConfidence

instance Commitable (Revelation BenchProposal) where
  type Commit (Revelation BenchProposal) = BenchHash

  commit (BenchRevelation benchProposal) = byronHash benchProposal

instance Identifiable (Voter BenchProposal) where
  data Id (Voter BenchProposal) = BenchVoterId BenchHash
    deriving (Ord, Eq, Show, Generic, NoThunks)

  _id = BenchVoterId . byronHash

instance Signed (Submission BenchProposal) where
  -- Verifying signatures will be spread across the voting period. Hence this
  -- operation is not critical for the update system performance. We are
  -- interested in measuring the performance of the tally process.
  --
  -- To measure the performance of the signing algorithms is better to do this
  -- in isolation, in a different place, since this is not related to system
  -- updates.
  signatureVerifies = const True

instance Signed (Vote BenchProposal) where
  -- For the same reason that we do not use a real signature for submissions, we
  -- do not use a signature for votes.
  signatureVerifies = const True

--------------------------------------------------------------------------------
-- Hashing algorithm to be used in the benchmarks
--------------------------------------------------------------------------------

type BenchHash = ShortByteString

-- Calculate the hash as it is done in Byron. See @module
-- Cardano.Chain.Common.AddressHash@ in @cardano-ledger@.
--
byronHash :: ToCBOR a => a -> BenchHash
byronHash = Short.toShort . ByteArray.convert . secondHash . firstHash
  where
    firstHash :: ToCBOR a => a -> Crypto.Digest Crypto.SHA3_256
    firstHash
      = Crypto.hash . BSL.toStrict . Builder.toLazyByteString
      . CBOR.Write.toBuilder . toCBOR
    secondHash :: Crypto.Digest Crypto.SHA3_256 -> Crypto.Digest Crypto.Blake2b_224
    secondHash = Crypto.hash
