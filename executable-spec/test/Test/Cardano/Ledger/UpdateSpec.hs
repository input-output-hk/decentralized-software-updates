{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions to operate on specifications of system updates (software,
-- protocol, parameters) etc, to be used in tests.
module Test.Cardano.Ledger.UpdateSpec where

import           Control.Monad.State (gets)
import           Data.Word (Word64)

import qualified Cardano.Ledger.Update as Update
import           Cardano.Ledger.Update.Proposal
import           Cardano.Ledger.Update.ProposalsState (Decision (Approved, Expired, Rejected, Undecided, WithNoQuorum))

import           Test.Cardano.Ledger.Update.Interface

import           Test.Cardano.Ledger.Update.TestCase

import           Test.Cardano.Ledger.Update.Data


-- | Specification of a test-case update. This contains all the information
-- required for an update to be activated, which includes:
--
-- * SIP payload
--
-- * Ideation payload
--
data UpdateSpec =
  UpdateSpec
  { getUpdateSpecId   :: !Word64
    -- ^ This should uniquely identify the update spec.
  , getSIPSubmission  :: !(Submission MockSIP)
  , getSIPRevelation  :: !(Revelation MockSIP)
  , getImplSubmission :: !(Submission MockImpl)
  , getImplRevelation :: !(Revelation MockImpl)
  } deriving Show

getProtocol :: UpdateSpec -> Protocol MockImpl
getProtocol updateSpec =
  case implementationType (getImpl updateSpec) of
    Protocol p -> p
    x          -> error $ "UpdateSpec does not contain a protocol: " ++ show x

getSIP :: UpdateSpec -> MockSIP
getSIP = reveals . getSIPRevelation

getImpl :: UpdateSpec -> MockImpl
getImpl = reveals . getImplRevelation

getSIPId :: UpdateSpec -> Id MockSIP
getSIPId = _id . getSIP

getImplId :: UpdateSpec -> Id MockImpl
getImplId = _id . getImpl

-- | Given protocol-update specification get its protocol version (the version
-- the system will upgrade to if it is applied).
--
-- Preconditions:
--
-- The update specification must refer to a protocol update, otherwise an error
-- will be thrown.
protocolVersion :: UpdateSpec -> Version (Protocol MockImpl)
protocolVersion updateSpec =
  case implementationType $ getImpl updateSpec of
    Protocol protocol -> version protocol
    someOtherImplType -> error $ "expecting a Protocol update, instead I got: "
                               ++ show someOtherImplType

dummyProtocolUpdateSpec
  :: Word64
  -- ^ Update spec id. See 'mkUpdate'.
  -> Participant
  -- ^ SIP author
  -> Participant
  -- ^ Implementation author signing key
  -> Protocol MockImpl
  -- ^ Protocol version that the update supersedes.
  -> Version (Protocol MockImpl)
  -- ^ New protocol version
  -> UpdateSpec
dummyProtocolUpdateSpec uId _sipAuthor _implAuthor supersedesProtocol newVersion =
  UpdateSpec
  { getUpdateSpecId   = uId
  , getSIPSubmission  = MockSubmission
                        { mpSubmissionCommit            = updateSpecCommit
                        , mpSubmissionSignatureVerifies = True
                        }
  , getSIPRevelation  = MockRevelation
                        { refersTo = updateSpecCommit
                        , reveals  = theSIP
                        }
  , getImplSubmission = MockSubmission
                        { mpSubmissionCommit            = updateSpecCommit
                        , mpSubmissionSignatureVerifies = True
                        }
  , getImplRevelation =  MockRevelation
                        { refersTo = updateSpecCommit
                        , reveals  = theImpl
                        }
  }
  where
    updateSpecCommit = MockCommit uId
    theSIP           = MockProposal
                       { mpId                   = MPId uId
                       -- TODO: we might want to make this configurable.
                       , mpVotingPeriodDuration = 10
                       , mpPayload              = ()
                       }
    theImpl          = MockProposal
                       { mpId                   = MPId uId
                       -- TODO: we might want to make this configurable.
                       , mpVotingPeriodDuration = 10
                       , mpPayload              = implInfo
                       }
    implInfo         =  ImplInfo
                        { mockImplements = _id theSIP
                        , mockImplType   = Protocol theProtocol
                        }
    theProtocol      = MockProtocol
                       { mpProtocolId        = ProtocolId uId
                       , mpProtocolVersion   = newVersion
                       , mpSupersedesId      = _id supersedesProtocol
                       , mpSupersedesVersion = version supersedesProtocol
                       }

mkUpdate
  :: Word64
  -- ^ Update spec id. This should uniquely identify the update spec. The number
  -- will be used to identify the SIP and implementation commits.
  -> Participant
  -- ^ SIP author. The implementation author will be calculated based on this
  -- participant by adding a constant to the participant id.
  -> (Version (Protocol MockImpl) -> Version (Protocol MockImpl))
  -> TestAction UpdateSpec
mkUpdate uId sipAuthor versionChange = do
  currentProtocol <- gets iStateCurrentVersion
  let newVersion = versionChange (version currentProtocol)
  pure $ mkUpdate' uId currentProtocol sipAuthor newVersion

mkUpdateThatDependsOn
  :: UpdateSpec
  -> Participant
  -> (Version (Protocol MockImpl) -> Version (Protocol MockImpl))
  -> UpdateSpec
mkUpdateThatDependsOn update participant versionChange =
  mkUpdate' newId (getProtocol update) participant newVersion
  where
    -- The way of getting a new is is brittle, but keeps the unit tests simple.
    newId      = getUpdateSpecId update + 1
    newVersion = versionChange $ version $ getProtocol update

mkUpdate'
  :: Word64
  -- ^ Update spec id. See 'mkUpdate'.
  -> Protocol MockImpl
  -- ^ Protocol that the update supersedes
  -> Participant
  -> Version (Protocol MockImpl)
  -> UpdateSpec
mkUpdate' uId supersedes sipAuthor newVersion =
  dummyProtocolUpdateSpec uId
                          sipAuthor
                          (sipAuthor `addToId` 10)
                          supersedes
                          newVersion

-- | Get the state of an update specification.
--
-- TODO: as the SIP's are removed from one state variable and placed into
-- another, this function might return an old state (e.g. SIP Submitted). We
-- should make sure this doesn't happen.
stateOf :: UpdateSpec -> IState MockSIP MockImpl -> UpdateState
stateOf updateSpec st
  | Update.isTheCurrentVersion protocolId st
    = Activated
  | Update.isScheduled protocolId st
    = Scheduled
  | Update.isBeingEndorsed protocolId st
    = BeingEndorsed
  | Update.isQueued protocolId st
    = Queued
  | Update.isDiscardedDueToBeing protocolId Update.Expired st
    = ActivationExpired
  | Update.isDiscardedDueToBeing protocolId Update.Canceled st
    = ActivationCanceled
  | Update.isDiscardedDueToBeing protocolId Update.Unsupported st
    = ActivationUnsupported
  | Update.isImplementationStably st implId Approved st
    = error "A stably approved implementation goes to the activation phase."
  | Update.isImplementation implId Approved st
    = error "An approved implementation goes to the activation phase."
  | Update.isImplementation implId Rejected st
    = Implementation (Is Rejected)
  | Update.isImplementation implId WithNoQuorum st
    = Implementation (Is WithNoQuorum)
  | Update.isImplementation implId Expired st
    = Implementation (Is Expired)
  | Update.isImplementation implId Undecided st
    = Implementation (Is Undecided)
  | Update.isImplementationStablyRevealed st implId st
    = Implementation StablyRevealed
  | Update.isImplementationRevealed implId st
    = Implementation Revealed
  | Update.isImplementationStablySubmitted st implCommit st
    = Implementation StablySubmitted
  | Update.isImplementationSubmitted implCommit st
    = Implementation Submitted
  | Update.isSIPStably st sipId Approved st
    = SIP (IsStably Approved)
  | Update.isSIP sipId Approved st
    = SIP (Is Approved)
  | Update.isSIPStablyRevealed st sipId st
    = SIP StablyRevealed
  | Update.isSIPRevealed sipId st
    = SIP  Revealed
  | Update.isSIPStablySubmitted st sipCommit st
    = SIP StablySubmitted
  | Update.isSIPSubmitted sipCommit (updateSt st)
    = SIP Submitted
  | otherwise
  = Unknown
  where
    sipId      = _id    (getSIP updateSpec)
    sipCommit  = revelationCommit (getSIPSubmission updateSpec)
    implId     = _id    (getImpl updateSpec)
    implCommit = revelationCommit (getImplSubmission updateSpec)
    protocolId = _id    (getProtocol updateSpec)
