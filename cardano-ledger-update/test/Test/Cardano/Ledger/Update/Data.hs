{-# LANGUAGE DerivingStrategies #-}
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

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR),
                     decodeListLenOf, encodeListLen)

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

  newtype NonProtocol MockImpl =
    MockNonProtocol { appId :: Word64 }
    deriving stock (Eq, Show)
    deriving newtype (ToCBOR, FromCBOR)

instance Activable (Protocol MockImpl) where
  newtype Endorser (Protocol MockImpl) = MockEndorser Participant
    deriving stock (Eq, Ord, Show)
    deriving newtype (ToCBOR, FromCBOR)

  newtype Version (Protocol MockImpl) = Version { getVersion :: Word64 }
    deriving stock (Eq, Ord, Show)
    deriving newtype (ToCBOR, FromCBOR)

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
    deriving stock (Eq, Ord, Show)
    deriving newtype (ToCBOR, FromCBOR)

  _id = mpProtocolId

nextId :: Id (Protocol MockImpl) -> Id (Protocol MockImpl)
nextId (ProtocolId i)
  | i == maxBound = error "Maximum number of id's was reached."
  | otherwise     = ProtocolId (i + 1)

instance Identifiable (NonProtocol MockImpl) where
  newtype Id (NonProtocol MockImpl) = AppId Word64
    deriving stock (Eq, Ord, Show)
    deriving newtype (ToCBOR, FromCBOR)

  _id = AppId . appId

instance Identifiable (Endorser (Protocol MockImpl)) where
  newtype Id (Endorser (Protocol MockImpl)) =
    EndorserId { unEndorserId :: Id Participant }
    deriving stock (Eq, Ord, Show)
    deriving newtype (ToCBOR, FromCBOR)

  _id (MockEndorser participant) = EndorserId (_id participant)

--------------------------------------------------------------------------------
-- Serialisation instances
--------------------------------------------------------------------------------

instance ToCBOR (Protocol MockImpl) where
  toCBOR p =  encodeListLen 4
           <> toCBOR (mpProtocolId p)
           <> toCBOR (mpProtocolVersion p)
           <> toCBOR (mpSupersedesId p)
           <> toCBOR (mpSupersedesVersion p)

instance FromCBOR (Protocol MockImpl) where
  fromCBOR = do
    decodeListLenOf 4
    pid <- fromCBOR
    pv  <- fromCBOR
    si  <- fromCBOR
    sv  <- fromCBOR
    return $! MockProtocol pid pv si sv

instance ToCBOR ImplInfo where
  toCBOR ii =  encodeListLen 2
            <> toCBOR (mockImplements ii)
            <> toCBOR (mockImplType ii)

instance FromCBOR ImplInfo where
  fromCBOR = do
    decodeListLenOf 2
    mi <- fromCBOR
    mt <- fromCBOR
    return $! ImplInfo mi mt
