{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Update.Data
  ( MockSIP
  , MockImpl
  , increaseVersion
  , Endorser (MockEndorser)
  , unEndorserId
  , Protocol (MockProtocol, mpProtocolId, mpProtocolVersion, mpSupersedesId, mpSupersedesVersion)
  , Id (ProtocolId, AppId, EndorserId, MPId)
  , nextId
  , Version (Version, getVersion)
  , ImplInfo (ImplInfo, mockImplements, mockImplType)
    -- * Re-exports from @MockProposal@
  , MockProposal.MockProposal (MockProposal, mpId, mpVotingPeriodDuration, mpPayload)
  , MockProposal.MockCommit (MockCommit)
  , MockProposal.Submission (MockSubmission, mpSubmissionCommit, mpSubmissionSignatureVerifies)
  , MockProposal.Revelation (MockRevelation, refersTo, reveals)
  , MockProposal.Participant (Participant)
  , MockProposal.addToId
  , MockProposal.Vote (MockVote)
  , MockProposal.Voter (MockVoter)
  , MockProposal.unMockVoterId
  , MockProposal.unParticipantId
  , MockProposal.mkParticipant
  )
where

import           Data.Word (Word64)

import           Cardano.Ledger.Update.Proposal

import           Test.Cardano.Ledger.Update.Data.MockProposal
import qualified Test.Cardano.Ledger.Update.Data.MockProposal as MockProposal

--------------------------------------------------------------------------------
-- Implementations
--------------------------------------------------------------------------------

type MockSIP = MockProposal SIP ()

data SIP = SIP
  deriving (Eq, Ord, Show)

data Impl = Impl
  deriving (Eq, Ord, Show)

type MockImpl = MockProposal Impl ImplInfo

deriving instance Eq (ImplementationType MockImpl)

data ImplInfo =
  ImplInfo
  { mockImplements :: !(Id MockSIP)
  , mockImplType   :: !(ImplementationType MockImpl)
  } deriving (Eq, Show)

instance Implementation MockSIP MockImpl where

  preProposalId      (MockProposal {mpPayload}) = mockImplements mpPayload
  implementationType (MockProposal {mpPayload}) = mockImplType mpPayload

  data Protocol MockImpl =
    MockProtocol
    { mpProtocolId        :: !(Id (Protocol MockImpl))
    , mpProtocolVersion   :: !(Version (Protocol MockImpl))
    , mpSupersedesId      :: !(Id (Protocol MockImpl))
    , mpSupersedesVersion :: !(Version (Protocol MockImpl))
    } deriving (Eq, Ord, Show)

  newtype Application MockImpl =
    MockApplication { appId :: Word64 }
    deriving (Eq, Show)

instance Activable (Protocol MockImpl) where
  newtype Endorser (Protocol MockImpl) = MockEndorser Participant
    deriving (Eq, Ord, Show)

  data Version (Protocol MockImpl) = Version { getVersion :: Word64 }
    deriving (Eq, Ord, Show)

  version           = mpProtocolVersion
  supersedesId      = mpSupersedesId
  supersedesVersion = mpSupersedesVersion

increaseVersion
  :: Version (Protocol MockImpl) -> Word64 -> Version (Protocol MockImpl)
increaseVersion (Version v) inc = Version (v + inc)

--------------------------------------------------------------------------------
-- Identifiable instances
--------------------------------------------------------------------------------

instance Identifiable (Protocol MockImpl) where
  newtype Id (Protocol MockImpl) = ProtocolId Word64
    deriving (Eq, Ord, Show)

  _id = mpProtocolId

nextId :: Id (Protocol MockImpl) -> Id (Protocol MockImpl)
nextId (ProtocolId i)
  | i == maxBound = error "Maximum number of id's was reached."
  | otherwise     = ProtocolId (i + 1)

instance Identifiable (Application MockImpl) where
  data Id (Application MockImpl) = AppId Word64
    deriving (Eq, Ord, Show)

  _id = AppId . appId

instance Identifiable (Endorser (Protocol MockImpl)) where
  newtype Id (Endorser (Protocol MockImpl)) =
    EndorserId { unEndorserId :: Id Participant }
    deriving (Eq, Ord, Show)

  _id (MockEndorser participant) = EndorserId (_id participant)
