{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions to operate on specifications of system updates (software,
-- protocol, parameters) etc, to be used in tests.
module Cardano.Ledger.UpdateSpec where

import           Control.Monad.State (gets)
import qualified Data.Text as Text

import           Cardano.Ledger.Spec.Classes.Hashable (Hash, hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (SKey)
import           Cardano.Ledger.Spec.State.ProposalState (Decision (Approved, Expired, Rejected, Undecided, WithNoQuorum))
import           Cardano.Ledger.Spec.STS.Update.Approval.Data (ProtocolVersion)
import           Cardano.Ledger.Spec.STS.Update.Approval.Data (addMajor,
                     addMinor)
import qualified Cardano.Ledger.Spec.STS.Update.Approval.Data as Approval.Data
import qualified Cardano.Ledger.Spec.STS.Update.Data as Update.Data
import           Cardano.Ledger.Spec.STS.Update.Data.Commit (Commit)
import qualified Cardano.Ledger.Spec.STS.Update.Ideation.Data as Ideation.Data

import qualified Cardano.Ledger.Update as Update
import qualified Cardano.Ledger.Update.Ideation as Ideation

import           Cardano.Ledger.Mock (Mock, vkeyFromSkey, wordToSKey)
import           Cardano.Ledger.Update.Interface

import           Cardano.Ledger.Update.TestCase


-- | Specification of a test-case update. This contains all the information
-- required for an update to be activated, which includes:
--
-- * SIP payload
--
-- * Ideation payload
--
data UpdateSpec =
  UpdateSpec
  { getSIPAuthorSKey  :: SKey Mock
  , getSIPCommit      :: Ideation.Data.SIPCommit Mock
  , getSIPHash        :: Ideation.Data.SIPHash Mock
  , getSIP            :: Ideation.Data.SIP Mock

  , getImplAuthorSKey :: SKey Mock
  , getImplCommit     :: Commit Mock (Approval.Data.Implementation Mock)
  , getImplSubmission :: Approval.Data.Submission Mock
  , getImpl           :: Approval.Data.Implementation Mock
  , getImplHash       :: Hash Mock (Approval.Data.ImplementationData Mock)
  } deriving Show

-- | Given protocol-update specification get its protocol version (the version
-- the system will upgrade to if it is applied).
--
-- Preconditions:
--
-- The update specification must refer to a protocol update, otherwise an error
-- will be thrown.
protocolVersion :: UpdateSpec -> ProtocolVersion
protocolVersion updateSpec =
  case Approval.Data.implType $ Approval.Data.implPayload $ getImpl updateSpec of
    Approval.Data.Protocol { Approval.Data.puVersion }
      -> puVersion
    someOtherImplType
      -> error $ "expecting a Protocol update, instead I got: "
               ++ show someOtherImplType

dummyProtocolUpdate
  :: SKey Mock
  -- ^ SIP author signing key
  -> SKey Mock
  -- ^ Implementation author signing key
  -> (ProtocolVersion, Hash Mock (Approval.Data.ImplementationData Mock))
  -- ^ Protocol version and hash that the update supersedes.
  -> ProtocolVersion
  -> UpdateSpec
dummyProtocolUpdate sipAuthorSKey implAuthorSKey supersedes aProtocolVersion =
  UpdateSpec
  { getSIPAuthorSKey  = sipAuthorSKey
  , getSIPCommit      = Ideation.Data.mkSIPCommit sipAuthorSKey theSIP
  , getSIPHash        = theSIPHash
  , getSIP            = theSIP

  , getImplAuthorSKey = implAuthorSKey
  , getImplCommit     = Approval.Data.commit theImplSubmission
  , getImplSubmission = theImplSubmission
  , getImpl           = theImpl
  , getImplHash       = hash $ Approval.Data.implPayload theImpl
  }
  where
    theSIP        =
      Ideation.Data.SIP
      { Ideation.Data.sipHash    = theSIPHash
      , Ideation.Data.sipAuthor  = theSIPAuthor
      , Ideation.Data.sipSalt    = 33
      , Ideation.Data.sipPayload = theSIPData
      }

    theSIPAuthor = vkeyFromSkey sipAuthorSKey
    theSIPData =
      Ideation.Data.SIPData
      { Ideation.Data.url =
        -- TODO: we use this workaround to distinguish SIP's. At the moment the
        -- SIPData does not include the author.
        Update.Data.URL $ "foo" <> Text.pack (show theSIPAuthor)
      , Ideation.Data.metadata =
        Ideation.Data.SIPMetadata
        -- TODO: version from to should be removed, and the impact parameters as well probably.
        { Ideation.Data.versionFrom       = (Ideation.Data.ProtVer 0, Ideation.Data.ApVer 0)
        , Ideation.Data.versionTo         =
          -- TODO: we need a way to distinguishing between SIPMetadata. This is
          -- just a workaround for the time being.
          ( Ideation.Data.ProtVer (fromIntegral $ Approval.Data.major aProtocolVersion)
          , Ideation.Data.ApVer   (fromIntegral $ Approval.Data.minor aProtocolVersion)
          )
        , Ideation.Data.impactsConsensus  = Ideation.Data.Impact
        , Ideation.Data.impactsParameters = []
        , Ideation.Data.votPeriodDuration = 100 -- TODO: we might want to make this a parameter!
        }
      }
    theSIPHash = hash theSIPData

    theImpl =
      Approval.Data.Implementation
      { Approval.Data.implAuthor  = vkeyFromSkey implAuthorSKey
      , Approval.Data.implSalt    = 732
      , Approval.Data.implPayload =
          Approval.Data.ImplementationData
          { Approval.Data.implDataSIPHash = theSIPHash
          , Approval.Data.implDataVPD     = 10 -- TODO: we might want to make this configurable
          , Approval.Data.implType        =
            Approval.Data.Protocol
            { Approval.Data.puSupersedes = Just supersedes
            , Approval.Data.puVersion    = aProtocolVersion
            , Approval.Data.puParameters = Approval.Data.ParametersUpdate
            , Approval.Data.puType       = Approval.Data.ParametersOnly
            }
          }
      }
    theImplSubmission = Approval.Data.mkSubmission
                           implAuthorSKey
                           theImpl


mkUpdate :: Word -> VersionChange -> TestAction UpdateSpec
mkUpdate i vc = do
  currentVersion <- gets iStateCurrentVersion
  let
    versionAndHash = ( Approval.Data.version  currentVersion
                     , Approval.Data.implHash currentVersion
                     )
  pure $ mkUpdate' versionAndHash i vc

data VersionChange = IncreaseMajor | IncreaseMinor
  deriving (Eq, Enum, Show)

mkUpdateThatDependsOn
  :: UpdateSpec
  -> Word
  -> VersionChange
  -> UpdateSpec
mkUpdateThatDependsOn update = mkUpdate' (supersedesVersion, supersedesHash)
  where
    updateImplData = Approval.Data.implPayload  (getImpl update)
    supersedesVersion = Approval.Data.implementationVersion updateImplData
    supersedesHash = getImplHash update

mkUpdate'
  :: (ProtocolVersion, Hash Mock (Approval.Data.ImplementationData Mock))
  -> Word
  -> VersionChange
  -> UpdateSpec
mkUpdate' (aProtocolVersion, aVersionHash) i vc =
  dummyProtocolUpdate (wordToSKey i)
                      (wordToSKey i + 10)
                      (aProtocolVersion, aVersionHash)
                      (bumpVersion vc aProtocolVersion)
  where
    bumpVersion IncreaseMajor = (`addMajor` 1)
    bumpVersion IncreaseMinor = (`addMinor` 1)

-- | Get the state of an update specification.
--
-- TODO: as the SIP's are removed from one state variable and placed into
-- another, this function might return an old state (e.g. SIP Submitted). We
-- should make sure this doesn't happen.
stateOf :: UpdateSpec -> IState Mock -> UpdateState
stateOf updateSpec st
  | Update.isTheCurrentVersion implHash st
    = Activated
  | Update.isScheduled implHash st
    = Scheduled
  | Update.isBeingEndorsed implHash st
    = BeingEndorsed
  | Update.isQueued implHash st
    = Queued
  | Update.isDiscardedDueToBeing implHash Update.Expired st
    = ActivationExpired
  | Update.isDiscardedDueToBeing implHash Update.Canceled st
    = ActivationCanceled
  | Update.isDiscardedDueToBeing implHash Update.Unsupported st
    = ActivationUnsupported
  | Update.isImplementationStably st implHash Approved st
    = error "A stably approved implementation goes to the activation phase."
  | Update.isImplementation implHash Approved st
    = error "An approved implementation goes to the activation phase."
  | Update.isImplementation implHash Rejected st
    = Implementation (Is Rejected)
  | Update.isImplementation implHash WithNoQuorum st
    = Implementation (Is WithNoQuorum)
  | Update.isImplementation implHash Expired st
    = Implementation (Is Expired)
  | Update.isImplementation implHash Undecided st
    = Implementation (Is Undecided)
  | Update.isImplementationStablyRevealed st implHash st
    = Implementation StablyRevealed
  | Update.isImplementationRevealed implHash st
    = Implementation Revealed
  | Update.isImplementationStablySubmitted st implCommit st
    = Implementation StablySubmitted
  | Update.isImplementationSubmitted implCommit st
    = Implementation Submitted
  | Update.isSIPStably st sipHash Approved st
    = SIP (IsStably Approved)
  | Ideation.isSIP sipHash Approved st
    = SIP (Is Approved)
  | Ideation.isSIPStablyRevealed st sipHash st
    = SIP StablyRevealed
  | Ideation.isSIPRevealed sipHash st
    = SIP  Revealed
  | Ideation.isSIPStablySubmitted st sipCommit st
    = SIP StablySubmitted
  | Update.isSIPSubmitted sipCommit (updateSt st)
    = SIP Submitted
  | otherwise
  = Unknown
  where
    sipHash    = getSIPHash updateSpec
    sipCommit  = Ideation.Data.commit $ getSIPCommit updateSpec
    implHash   = getImplHash updateSpec
    implCommit = getImplCommit updateSpec
