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

import qualified Control.DeepSeq as Deep
import           Data.Either (isRight)
import           Data.List (foldr, map, repeat, take, zip)
import           Data.List (foldl')
import qualified Data.Map.Strict as Map
import           Data.Serialize (encode)
import           GHC.Generics (Generic)

import qualified Codec.CBOR.Write as CBOR.Write
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL

import           Cardano.Binary (ToCBOR, toCBOR)
import           Cardano.Crypto.DSIGN.Class (SignedDSIGN)
import qualified Cardano.Crypto.DSIGN.Class as Crypto.DSIGN
import           Cardano.Crypto.DSIGN.Mock (MockDSIGN)
import           Cardano.Crypto.DSIGN.Mock (SignKeyDSIGN (SignKeyMockDSIGN),
                     VerKeyDSIGN (VerKeyMockDSIGN))
import qualified Cardano.Crypto.DSIGN.Mock as Crypto.Mock
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     SKey, Signable, Signature, VKey, sign, verify)
import           Ledger.Core (BlockCount (BlockCount), Slot (Slot),
                     SlotCount (SlotCount), (*.), (+.))

import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (VKey)
import           Cardano.Ledger.Spec.State.ProposalsState (ProposalsState,
                     decision, revealProposal, tally, updateBallot)
import           Cardano.Ledger.Spec.State.ProposalState (Decision (Accepted, Expired, NoQuorum, Rejected, Undecided),
                     HasVotingPeriod, IsVote, VotingPeriod (VotingPeriod),
                     getConfidence, getVoter, getVotingPeriodDuration)
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution,
                     fromList, totalStake)
import           Cardano.Ledger.Spec.STS.Update.Data
                     (Confidence (Abstain, Against, For), Stake (Stake))
import           Cardano.Ledger.Test.Mock (Mock)


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
  { stakeDist      :: !(StakeDistribution p)
  , proposals      :: ![Proposal]
  -- ^ Update proposals that are active at the same time. We assume their voting
  -- period overlaps exactly, which is the worst case.
  , proposalHashes :: ![Hash BenchCrypto Proposal]
  -- ^ Proposal hashes. These must correspond to 'proposals'. We use this to
  -- avoid computing hashes when getting the results of the tally.
  , participants   :: ![VKey BenchCrypto]
  }
  deriving (Show, Generic)

-- | Type of proposals we're voting on in the benchmarks
newtype Proposal = Proposal Word
  deriving stock (Show, Eq, Generic)
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
  { issuer     :: VKey BenchCrypto
  , confidence :: Confidence
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
  foldl' reveal mempty proposals
  where
   -- For the purposes of benchmarking the tally process we only need a single
   -- voting period.
    reveal st i = revealProposal revelationSlot (VotingPeriod 1) i st

-- | Vote on all the proposals in the state.
voteOnProposals
  :: TallyData BenchCrypto Proposal
  -> ProposalsState BenchCrypto Proposal
  -> ProposalsState BenchCrypto Proposal
voteOnProposals TallyData {participants, proposals} st =
  foldl' voteOnProposal st proposalsHashes
  where
    voteOnProposal st' hp   =
      foldl' vote st' participants
      where
        vote st'' who = updateBallot hp (Vote who For) st''
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
    proposalsStateAfterTally = tally k stableAt stakeDist r_a proposalsState
      where
        -- Here we're assuming a vote period duration of 1, as defined in the
        -- 'HasVotingPeriod' instance of 'Vote'.
        stableAt = revelationSlot
                 +. 2 *. k                         -- Revelation is stable
                 +. benchmarksVotingPeriodDuration -- Voting period ended
                 +. 2 *. k                         -- End of the voting period
                                                   -- is stable: tally can take
                                                   -- place.

newtype NumberOfParticipants = NumberOfParticipants Word

newtype NumberOfConcurrentUPs = NumberOfConcurrentUPs Word

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
    tallyData =
      TallyData
      { stakeDist       = mkStakeDist participantsHashes
      , proposals       = proposals'
      , proposalHashes  = hash <$> proposals'
      , participants    = participants'
      }
    proposals'          = mkProposal <$> [1.. numOfConcurrentUPs]
    proposalsState      = voteOnProposals tallyData
                        $ revealProposals constants tallyData
    participants'       = VerKeyMockDSIGN <$> [1 .. fromIntegral numOfParticipants]
    !participantsHashes = hash <$> participants'


-- | Uniform stake distribution with a stake of 1 for each stakeholder.
mkStakeDist :: [Hash BenchCrypto (VKey BenchCrypto)] -> StakeDistribution BenchCrypto
mkStakeDist participants
  = fromList
  $ zip participants
        (repeat (Stake 1))

--------------------------------------------------------------------------------
-- Hashing, signing, and verification algorithms to be used in the benchmarks
--------------------------------------------------------------------------------

data BenchCrypto

instance Hashable BenchCrypto where

  newtype Hash BenchCrypto a = BenchHash (Crypto.Digest Crypto.Blake2b_256)
    deriving (Eq, Ord, Show, ToCBOR)

  type HasHash BenchCrypto = ToCBOR

  hash = BenchHash
       . Crypto.hash
       . BSL.toStrict
       . Builder.toLazyByteString
       . CBOR.Write.toBuilder
       . toCBOR

instance HasSigningScheme BenchCrypto where

  newtype Signature BenchCrypto a = BenchCryptoSignature (SignedDSIGN MockDSIGN a)
    deriving (Eq, Show)

  type VKey BenchCrypto = Crypto.Mock.VerKeyDSIGN MockDSIGN

  type SKey BenchCrypto = Crypto.Mock.SignKeyDSIGN MockDSIGN

  type Signable BenchCrypto = Crypto.DSIGN.Signable MockDSIGN

  sign a skey = BenchCryptoSignature $ Crypto.Mock.mockSigned a skey

  verify vkey a (BenchCryptoSignature sig) =
    isRight $ Crypto.DSIGN.verifySignedDSIGN @MockDSIGN () vkey a sig
