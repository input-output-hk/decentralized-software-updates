{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Benchmarks.Update.Tally where

import           Cardano.Prelude (NoUnexpectedThunks)

import qualified Control.DeepSeq as Deep
import           Control.Monad ((<$!>))
import           Data.Either (isRight)
import           Data.List (repeat, zip)
import           Data.List (foldl')
import           GHC.Generics (Generic)

import qualified Codec.CBOR.Write as CBOR.Write
import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short

import           Cardano.Binary (ToCBOR, toCBOR)
import           Cardano.Crypto.DSIGN.Class (SignedDSIGN)
import qualified Cardano.Crypto.DSIGN.Class as Crypto.DSIGN
import           Cardano.Crypto.DSIGN.Mock (MockDSIGN)
import           Cardano.Crypto.DSIGN.Mock (VerKeyDSIGN (VerKeyMockDSIGN))
import qualified Cardano.Crypto.DSIGN.Mock as Crypto.Mock
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     SKey, Signable, Signature, VKey, sign, verify)
import           Ledger.Core (BlockCount, Slot, SlotCount, (*.), (+.))

import           Cardano.Ledger.Spec.Classes.HasAdversarialStakeRatio
                     (HasAdversarialStakeRatio)
import qualified Cardano.Ledger.Spec.Classes.HasAdversarialStakeRatio
import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)
import           Cardano.Ledger.Spec.Classes.HasStakeDistribution
                     (HasStakeDistribution,
                     TechnicalExperts (TechnicalExperts))
import qualified Cardano.Ledger.Spec.Classes.HasStakeDistribution
import           Cardano.Ledger.Spec.Classes.TracksSlotTime (TracksSlotTime)
import qualified Cardano.Ledger.Spec.Classes.TracksSlotTime
import           Cardano.Ledger.Spec.State.ProposalsState (ProposalsState,
                     decision, reveal, tally, updateBallot')
import           Cardano.Ledger.Spec.State.ProposalState (Decision,
                     HasVotingPeriod, IsVote, VotingPeriod (VotingPeriod),
                     getConfidence, getVoter, getVotingPeriodDuration)
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution,
                     fromList)
import           Cardano.Ledger.Spec.STS.Update.Data (Confidence (For),
                     Stake (Stake))


data BenchmarkConstants =
  BenchmarkConstants
  { k                :: !BlockCount
  -- ^ Chain stability parameter.
  , r_a              :: !Float
  -- ^ Adversarial stake ratio.
  , revelationSlot   :: !Slot
  -- ^ Slot at which __all__ proposals were registered.
  }
  deriving (Show, Eq)

-- | Data required for performing tallying the votes.
data TallyData p d =
  TallyData
  { stakeDist          :: !(StakeDistribution p)
  , proposals          :: ![Proposal]
  -- ^ Update proposals that are active at the same time. We assume their voting
  -- period overlaps exactly, which is the worst case.
  , proposalHashes     :: ![Hash BenchCrypto Proposal]
  -- ^ Proposal hashes. These must correspond to 'proposals'. We use this to
  -- avoid computing hashes when getting the results of the tally.
  , participantsHashes :: ![Hash BenchCrypto (VerKeyDSIGN MockDSIGN)]
  }
  deriving (Show, Generic)

instance NoUnexpectedThunks (Hash p (VKey p)) => NoUnexpectedThunks (TallyData p d)

-- | Type of proposals we're voting on in the benchmarks
newtype Proposal = Proposal Word
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NoUnexpectedThunks)
  deriving newtype (ToCBOR, Deep.NFData)

mkProposal :: Word -> Proposal
mkProposal = Proposal

instance HasVotingPeriod Proposal where
  -- We assume all proposals to last for 1 slot.
  getVotingPeriodDuration = const benchmarksVotingPeriodDuration

-- | Voting period duration that is used throughout the module.
benchmarksVotingPeriodDuration :: SlotCount
benchmarksVotingPeriodDuration = 1

data Vote =
  Vote
  { issuer     :: !(VKey BenchCrypto)
  , confidence :: !Confidence
  } deriving (Eq, Show)

instance IsVote BenchCrypto Vote where
  getVoter = issuer

  getConfidence = confidence

deriving instance ( Eq (StakeDistribution p)
                  , Hashable p
                  )
                  => Eq (TallyData p d)

-- | Simulate the revelation of the number of proposals given be the benchmark
-- parameters.
revealProposals
  :: BenchmarkConstants
  -> TallyData BenchCrypto Proposal
  -> ProposalsState BenchCrypto Proposal
revealProposals BenchmarkConstants { revelationSlot } TallyData { proposals } =
  foldl' revealProposal mempty proposals
  where
   -- For the purposes of benchmarking the tally process we only need a single
   -- voting period.
    revealProposal st i = reveal revelationSlot (VotingPeriod 1) i st

-- | Vote on all the proposals in the state.
voteOnProposals
  :: TallyData BenchCrypto Proposal
  -> ProposalsState BenchCrypto Proposal
  -> ProposalsState BenchCrypto Proposal
voteOnProposals TallyData {participantsHashes, proposals} st =
  foldl' voteOnProposal st proposalsHashes
  where
    voteOnProposal st' hp   =
      foldl' vote st' participantsHashes
      where
        vote st'' voterKeyHash = updateBallot' hp voterKeyHash For st''
    proposalsHashes = fmap hash proposals

-- | Get number of participants and run the tally
runTally
  :: BenchmarkConstants
  -> (TallyData BenchCrypto Proposal, ProposalsState BenchCrypto Proposal)
  -> [Decision]
runTally
  BenchmarkConstants { k, r_a, revelationSlot }
  (TallyData { stakeDist, proposalHashes }, proposalsState)
  = fmap (`decision` proposalsStateAfterTally) proposalHashes
  where
    proposalsStateAfterTally = tally env TechnicalExperts proposalsState
      where
        env = TallyEnv
              { envK = k
              , envCurrentSlot = currentSlot
              , envStakeDist = stakeDist
              , envAdversarialStakeRatio = r_a
              }
        -- The current slot should be set to a value that causes tally to take
        -- place without the voting period to become expired.
        --
        -- Here we're assuming a vote period duration of 1, as defined in the
        -- 'HasVotingPeriod' instance of 'Vote'.
        currentSlot = revelationSlot
                    +. 2 *. k                         -- Revelation is stable
                    +. benchmarksVotingPeriodDuration -- Voting period ended
                    +. 2 *. k                         -- End of the voting
                                                      -- period is stable: tally
                                                      -- can take place.

data TallyEnv =
  TallyEnv
  { envK :: BlockCount
  , envCurrentSlot :: Slot
  , envStakeDist :: StakeDistribution BenchCrypto
  , envAdversarialStakeRatio :: Float
  }

instance TracksSlotTime TallyEnv where
  stableAfter = (2 *. ) . envK

  currentSlot = envCurrentSlot

  slotsPerEpoch = error "Benchmarks should not use this"

  epochFirstSlot = error "Benchmarks should not use this"

instance HasStakeDistribution TechnicalExperts TallyEnv BenchCrypto where
  stakeDistribution TechnicalExperts = envStakeDist

instance HasAdversarialStakeRatio TallyEnv where
  adversarialStakeRatio = envAdversarialStakeRatio

newtype NumberOfParticipants = NumberOfParticipants Word

newtype NumberOfConcurrentUPs = NumberOfConcurrentUPs Word

-- | Create the data used for the benchmarks. It fully evaluates the data.
createTallyData
  :: BenchmarkConstants
  -> NumberOfParticipants
  -> NumberOfConcurrentUPs
  -> (TallyData BenchCrypto Proposal, ProposalsState BenchCrypto Proposal)
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
      , proposalHashes     = forceList $ hash <$!> proposals'
      , participantsHashes = theParticipantsHashes
      }
    proposals'            = mkProposal <$!> [1.. numOfConcurrentUPs]
    !proposalsState       =  voteOnProposals tallyData
                          $! revealProposals constants tallyData
    participants          = VerKeyMockDSIGN <$!> [1 .. fromIntegral numOfParticipants]
    theParticipantsHashes = hash <$!> participants


forceList :: [a] -> [a]
forceList xs = forceElements xs `seq` xs

forceElements :: [a] -> ()
forceElements = foldr seq ()

-- | Uniform stake distribution with a stake of 1 for each stakeholder.
mkStakeDist :: [Hash BenchCrypto (VKey BenchCrypto)] -> StakeDistribution BenchCrypto
mkStakeDist participantsHashes
  = fromList
  $ zip participantsHashes
        (repeat (Stake 1))

--------------------------------------------------------------------------------
-- Hashing, signing, and verification algorithms to be used in the benchmarks
--------------------------------------------------------------------------------

data BenchCrypto

instance Hashable BenchCrypto where

  newtype Hash BenchCrypto a = BenchHash ShortByteString
    deriving stock    (Eq, Ord, Show, Generic)
    deriving anyclass (NoUnexpectedThunks)

  type HasHash BenchCrypto = ToCBOR

  -- Calculate the hash as it is done in Byron. See @module
  -- Cardano.Chain.Common.AddressHash@ in @cardano-ledger@.
  --
  hash
    = BenchHash
    . Short.toShort
    . ByteArray.convert
    . secondHash
    . firstHash
    where
      firstHash :: ToCBOR a => a -> Crypto.Digest Crypto.SHA3_256
      firstHash
        = Crypto.hash
        . BSL.toStrict
        . Builder.toLazyByteString
        . CBOR.Write.toBuilder
        . toCBOR
      secondHash :: Crypto.Digest Crypto.SHA3_256 -> Crypto.Digest Crypto.Blake2b_224
      secondHash = Crypto.hash


instance HasSigningScheme BenchCrypto where

  newtype Signature BenchCrypto a = BenchCryptoSignature (SignedDSIGN MockDSIGN a)
    deriving stock    (Eq, Show, Generic)
    deriving anyclass (NoUnexpectedThunks)

  type VKey BenchCrypto = Crypto.Mock.VerKeyDSIGN MockDSIGN

  type SKey BenchCrypto = Crypto.Mock.SignKeyDSIGN MockDSIGN

  type Signable BenchCrypto = Crypto.DSIGN.Signable MockDSIGN

  sign a skey = BenchCryptoSignature $ Crypto.Mock.mockSigned a skey

  verify vkey a (BenchCryptoSignature sig) =
    isRight $ Crypto.DSIGN.verifySignedDSIGN @MockDSIGN () vkey a sig
