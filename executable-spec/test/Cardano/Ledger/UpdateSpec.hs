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
import qualified Cardano.Ledger.Spec.State.ActivationState as Activation
import           Cardano.Ledger.Spec.State.ApprovedSIPs (isSIPApproved,
                     whenSIPApproved)
import           Cardano.Ledger.Spec.State.ProposalState (Decision (Approved, Expired, Rejected, Undecided, WithNoQuorum))
import qualified Cardano.Ledger.Spec.STS.Update.Approval as Approval
import           Cardano.Ledger.Spec.STS.Update.Approval.Data (ProtocolVersion)
import           Cardano.Ledger.Spec.STS.Update.Approval.Data (addMajor,
                     addMinor)
import qualified Cardano.Ledger.Spec.STS.Update.Approval.Data as Approval.Data
import qualified Cardano.Ledger.Spec.STS.Update.Data as Update.Data
import           Cardano.Ledger.Spec.STS.Update.Data.Commit (Commit)
import qualified Cardano.Ledger.Spec.STS.Update.Ideation as Ideation
import qualified Cardano.Ledger.Spec.STS.Update.Ideation.Data as Ideation.Data

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
    theSIPHash = Ideation.Data.SIPHash $ hash theSIPData

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
  | Activation.isTheCurrentVersion implHash (iStateActivation st)
    = Activated
  | Activation.isScheduled implHash (iStateActivation st)
    = Scheduled
  | Activation.isBeingEndorsed implHash (iStateActivation st)
    = BeingEndorsed
  | Activation.isQueued implHash (iStateActivation st)
    = Queued
  | Activation.isDiscardedDueToBeing implHash Activation.Expired (iStateActivation st)
    = ActivationExpired
  | Activation.isDiscardedDueToBeing implHash Activation.Canceled (iStateActivation st)
    = ActivationCanceled
  | Activation.isDiscardedDueToBeing implHash Activation.Unsupported (iStateActivation st)
    = ActivationUnsupported
  | Approval.isStably st implHash Approved (iStateApproval st)
    = error "A stably approved implementation goes to the activation phase."
  | Approval.is implHash Approved (iStateApproval st)
    = error "An approved implementation goes to the activation phase."
  | Approval.is implHash Rejected (iStateApproval st)
    = Implementation (Is Rejected)
  | Approval.is implHash WithNoQuorum (iStateApproval st)
    = Implementation (Is WithNoQuorum)
  | Approval.is implHash Expired (iStateApproval st)
    = Implementation (Is Expired)
  | Approval.is implHash Undecided (iStateApproval st)
    = Implementation (Is Undecided)
  | Approval.isStablyRevealed st implHash (iStateApproval st)
    = Implementation StablyRevealed
  | Approval.isRevealed implHash (iStateApproval st)
    = Implementation Revealed
  | Approval.isStablySubmitted st implCommit (iStateApproval st)
    = Implementation StablySubmitted
  | Approval.isSubmitted implCommit (iStateApproval st)
    = Implementation Submitted
  | isStable' (whenSIPApproved sipHash (iStateApprvsips st))
    = SIP (IsStably Approved)
  | isSIPApproved sipHash (iStateApprvsips st)
    = SIP (Is Approved)
  | Ideation.isStablyRevealed (projectToIdeationEnv st) sipHash
    = SIP StablyRevealed
  | Ideation.isRevealed (projectToIdeationSt st) sipHash
    = SIP  Revealed
  | Ideation.isStablySubmitted (projectToIdeationEnv st) (projectToIdeationSt st) sipCommit
    = SIP StablySubmitted
  | Ideation.isSubmitted (projectToIdeationSt st) sipCommit
    = SIP Submitted
  | otherwise
  = Unknown
  where
    isStable' Nothing     = False
    isStable' (Just slot) = isStable slot st

    sipHash = getSIPHash updateSpec

    sipCommit = Ideation.Data.commit $ getSIPCommit updateSpec

    implHash = getImplHash updateSpec

    implCommit = getImplCommit updateSpec
