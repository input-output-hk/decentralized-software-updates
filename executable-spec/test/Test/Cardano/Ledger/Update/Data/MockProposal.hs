{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Update.Data.MockProposal where

import           Data.Word (Word64)

import           Cardano.Slotting.Slot (SlotNo)

import           Cardano.Ledger.Update.Proposal


--------------------------------------------------------------------------------
-- Participants
--------------------------------------------------------------------------------

newtype Participant = Participant (Id Participant)
  deriving (Eq, Ord, Show)

mkParticipant :: Word64 -> Participant
mkParticipant = Participant . ParticipantId

addToId :: Participant -> Word64 -> Participant
addToId (Participant (ParticipantId n)) = mkParticipant . (n+)

--------------------------------------------------------------------------------
-- Mock proposals
--------------------------------------------------------------------------------

-- | Proposal with additional payload
data MockProposal prop payload =
  MockProposal
  { mpId                   :: !(Id (MockProposal prop payload))
  , mpVotingPeriodDuration :: !SlotNo
  , mpPayload              :: !payload
  }
  deriving (Eq, Ord, Show)

instance Identifiable (MockProposal prop payload) where
  newtype Id (MockProposal prop payload) = MPId Word64
    deriving (Ord, Eq, Show)

  _id = mpId

instance Show payload => Proposal (MockProposal prop payload) where
  data Submission (MockProposal prop payload) =
    MockSubmission
    { mpSubmissionCommit            :: MockCommit
    , mpSubmissionSignatureVerifies :: Bool
    } deriving (Eq, Ord, Show)

  data Revelation (MockProposal prop payload) =
    MockRevelation
    { refersTo :: MockCommit
    , reveals  :: MockProposal prop payload
    } deriving (Eq, Ord, Show)

  revelationCommit = mpSubmissionCommit

  proposal = reveals

  votingPeriodDuration = mpVotingPeriodDuration

  data Vote (MockProposal prop payload) =
    MockVote
    { voteVoterId           :: Id (Voter (MockProposal prop payload))
    , voteCandidate         :: Id (MockProposal prop payload)
    , voteConfidence        :: Confidence
    , voteSignatureVerifies :: Bool
    } deriving (Show)

  data Voter (MockProposal prop payload) = MockVoter Participant
    deriving (Eq, Ord, Show)

  voter = voteVoterId
  candidate = voteCandidate
  confidence = voteConfidence

--------------------------------------------------------------------------------
-- Identifiable instances
--------------------------------------------------------------------------------

instance Identifiable Participant where
  newtype Id Participant = ParticipantId { unParticipantId :: Word64 }
    deriving (Eq, Ord, Show)

  _id (Participant pId) = pId

instance Identifiable  (Voter (MockProposal prop payload)) where
  newtype Id (Voter (MockProposal prop payload)) = MockVoterId (Id Participant)
    deriving (Ord, Eq, Show)

  _id (MockVoter participant) = MockVoterId (_id participant)

--------------------------------------------------------------------------------
-- Signed instances
--------------------------------------------------------------------------------

instance Signed (Submission (MockProposal prop payload)) where
  signatureVerifies = mpSubmissionSignatureVerifies

instance Signed (Vote (MockProposal prop payload)) where
  signatureVerifies = voteSignatureVerifies

--------------------------------------------------------------------------------
-- Commits
--------------------------------------------------------------------------------

newtype MockCommit = MockCommit Word64
  deriving (Eq, Ord, Show)

instance Commitable (Revelation (MockProposal prop payload)) where
  type Commit (Revelation (MockProposal prop payload)) = MockCommit
  commit = refersTo
