{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Spec.STS.Update.Approval.Data where

import           Data.Maybe (fromMaybe)
import           Data.Typeable (Typeable)
import           Data.Word (Word16)
import           GHC.Generics (Generic)

import           Cardano.Binary (ToCBOR (toCBOR), encodeListLen, encodeWord)

import           Cardano.Ledger.Assert (assert, orElseShow)
import           Cardano.Ledger.Spec.Classes.HasAuthor (HasAuthor, author)
import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable)
import           Cardano.Ledger.Spec.Classes.HasSalt (HasSalt, salt)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     SKey, Signable, Signature, Signed, SignedPayload, VKey,
                     payloadSignature, sign, signatureVerifies, signedBy,
                     signedPayload)
import           Cardano.Ledger.Spec.State.ProposalsState (ProposalsState)
import           Cardano.Ledger.Spec.State.ProposalState (HasVotingPeriod,
                     IsVote, getConfidence, getVoter, getVotingPeriodDuration)
import           Cardano.Ledger.Spec.STS.Update.Data (Confidence, URL)
import           Cardano.Ledger.Spec.STS.Update.Data.Commit (Commit, calcCommit)
import           Cardano.Ledger.Spec.STS.Update.Ideation.Data (SIPData, SIPHash)
import           Ledger.Core (SlotCount)


type IPSSt p = ProposalsState p (ImplementationData p)

--------------------------------------------------------------------------------
-- STS data
--------------------------------------------------------------------------------

data Payload p
  = Submit (Submission p)
  | Reveal (Implementation p)
  | Vote (ImplVote p)
  deriving (Generic, Typeable)

data Submission p =
  Submission
  { commit  :: !(Commit p (Implementation p))
  , sAuthor :: !(VKey p)
  , sig     :: !(Signature p (Commit p (Implementation p)))
  }
  deriving (Generic, Typeable)

deriving instance (Hashable p, HasSigningScheme p) => Show (Submission p)

instance ( HasSigningScheme p
         , Signable p (SignedPayload (Submission p))
         ) => Signed p (Submission p) where

  type SignedPayload (Submission p) =
    Commit p (Implementation p)

  signedPayload = commit

  signedBy = sAuthor

  payloadSignature = sig

mkSubmission
  :: ( Hashable p
     , HasHash p (Implementation p)
     , HasHash p (Int, VKey p, Hash p (Implementation p))
     , Signable p (Commit p (Implementation p))
     , HasSigningScheme p
     )
  => SKey p
  -- ^ Key signing the submission.
  -> Implementation p
  -> Submission p
mkSubmission submitterSKey implementation =
  assert (signatureVerifies submission `orElseShow` "Created signature does not verify")
         submission
  where
    submission =
      Submission
      { commit  = commit'
      , sAuthor = implAuthor implementation
      , sig     = sign commit' submitterSKey
      }
    commit' = calcCommit implementation

deriving instance (Hashable p, HasSigningScheme p) => Show (Payload p)

-- | Software update-proposal implementation.
data Implementation p =
  Implementation
  { implAuthor  :: !(VKey p)
  , implSalt    :: !Int
  , implPayload :: !(ImplementationData p)
  } deriving (Generic)

deriving instance (Hashable p, Eq (VKey p)) => Eq (Implementation p)
deriving instance (Hashable p, Show (VKey p)) => Show (Implementation p)

instance HasAuthor (Implementation p) p where
  author = implAuthor

instance HasSalt (Implementation p) where
  salt = implSalt

instance HasVotingPeriod (Implementation p) where
  getVotingPeriodDuration = getVotingPeriodDuration . implPayload

data ImplementationData p =
  ImplementationData
  { implDataSIPHash :: !(SIPHash p)
    -- ^ Reference to the SIP that is being implemented.
  , implDataVPD     :: !SlotCount
    -- ^ Vote period duration for the implementation proposal.
  , implType        :: !(UpdateType p)
  } deriving (Eq, Ord, Show, Generic)

isApplicationUpdate :: ImplementationData p -> Bool
isApplicationUpdate (implType -> Application {}) = True
isApplicationUpdate _                            = False

data UpdateType p
  = Cancellation
    { toCancel :: ![Hash p (ImplementationData p)] }
  | Protocol
    { puSupersedes :: !(Maybe (ProtocolVersion, Hash p (ImplementationData p)))
      -- ^ Protocol version and hash of the proposal that the update supersedes.
      -- This field is @Nothing@ if the update is the first version run in the
      -- blockchain.
      --
      -- TODO: consider using strict Maybe and Tuple. This requires adding
      -- 'HasTypeReps' instances for these strict variants.
    , puVersion     :: !ProtocolVersion
      -- ^ Protocol version that the update will upgrade to.
    , puParameters  :: !ParametersUpdate
      -- ^ Description of the parameters update.
    , puType        :: !ProtocolUpdateType
    }
    -- ^ TODO: check invariant
    --
    -- > supersedes < version
    --
  | Application
    { auURL      :: !URL
      -- ^ Where to get the implementation data from.
    , auImplHash :: !ImplHash
      -- ^ Once the data is downloaded from the given URL, this hash should be
      -- used to check the data integrity.
    }
    -- ^ Application updates are not related to the protocol.
    deriving (Generic)

deriving instance Hashable p => Eq (UpdateType p)
deriving instance Hashable p => Ord (UpdateType p)
deriving instance Hashable p => Show (UpdateType p)

data ImplementationAndHash p =
  ImplementationAndHash
  { implHash :: !(Hash p (ImplementationData p))
  , implData :: !(ImplementationData p)
  } deriving (Generic)

deriving instance Hashable p => Show (ImplementationAndHash p)
deriving instance Hashable p => Eq (ImplementationAndHash p)
deriving instance Hashable p => Ord (ImplementationAndHash p)

version ::  Hashable p => ImplementationAndHash p -> ProtocolVersion
version = implementationVersion . implData

supersedes :: Hashable p => ImplementationAndHash p -> ProtocolVersion
supersedes = implementationSupersedes . implData

genesisUpdateType
  :: URL
  -> ImplHash
  -> UpdateType p
genesisUpdateType genesisImplURL genesisImplHash =
  Protocol
  { puSupersedes = Nothing
  , puVersion    = ProtocolVersion 0 1
  , puParameters = ParametersUpdate
  , puType =
      SoftwareProtocol
      { spuURL  = genesisImplURL
      , spuHash = genesisImplHash
      }
  }

data ProtocolUpdateType
  = ParametersOnly
  | SoftwareProtocol
    { spuURL  :: !URL
    , spuHash :: !ImplHash
    }
  deriving (Eq, Ord, Show, Generic)

-- | TODO: we need to define how to describe this
data ParametersUpdate = ParametersUpdate
  deriving (Eq, Ord, Show, Generic)

-- | Get the protocol version that the implementation supersedes.
--
-- Preconditions:
--
-- - The implementation must refer to a protocol update proposal. Otherwise an
--   error will be thrown.
implementationSupersedes :: Hashable p => ImplementationData p -> ProtocolVersion
implementationSupersedes (implType -> Protocol { puSupersedes }) =
  let errorMsg =
        "The implementation does not declare the version that it supersedes"
  in fst $ fromMaybe (error errorMsg) puSupersedes
implementationSupersedes someImplData                            =
  error $ "Implementation does not refer to an update proposal: "
        ++ show someImplData

-- | Get the version that the proposal implements.
implementationVersion ::  Hashable p => ImplementationData p -> ProtocolVersion
implementationVersion  (implType -> Protocol { puVersion }) = puVersion
implementationVersion someImplData                          =
  error $ "Implementation does not refer to an update proposal: "
        ++ show someImplData

-- TODO: write proper documentation
data ProtocolVersion =
  ProtocolVersion
  { major :: !Word16
  , minor :: !Word16
  }
  deriving (Eq, Ord, Show, Generic)

addMajor :: ProtocolVersion -> Word16 -> ProtocolVersion
addMajor pv@ProtocolVersion{ major } inc = pv { major = major + inc }

addMinor :: ProtocolVersion -> Word16 -> ProtocolVersion
addMinor pv@ProtocolVersion{ minor } inc = pv { minor = minor + inc }

implSIPHash :: Implementation p -> SIPHash p
implSIPHash = implDataSIPHash . implPayload

type ImplHash = Int -- TODO: we need to define what this will be.

instance HasVotingPeriod (ImplementationData p) where
  getVotingPeriodDuration = implDataVPD

data ImplVote p =
  ImplVote
    { vImplHash  :: !(Hash p (ImplementationData p))
      -- ^ Implementation that is being voted for.
    , confidence :: !Confidence
    , vAuthor    :: !(VKey p) -- TODO: Make this consistent with
                              -- 'Cardano.Ledger.Spec.STS.Update.Ideation.Data'.
                              -- There the corresponding field is called
                              -- 'voter'.
    , vSig       :: !(Signature p (SignedVoteData p))
    }
  deriving (Generic, Typeable)

type SignedVoteData p = ( Hash p (ImplementationData p)
                        , Confidence
                        , VKey p
                        )

mkImplVote
  :: ( HasSigningScheme p
     , Signable p (Hash p (ImplementationData p), Confidence, VKey p)
     )
  => SKey p
  -> VKey p
  -> Confidence
  -> Hash p (ImplementationData p)
  -> ImplVote p
mkImplVote voterSKey voterVKey confidence' implDataHash =
  assert (signatureVerifies vote `orElseShow` "Created signature does not verify")
         vote
  where
    vote =
      ImplVote
      { vImplHash  = implDataHash
      , confidence = confidence'
      , vAuthor    = voterVKey
      , vSig       = sign (implDataHash, confidence', voterVKey) voterSKey
      }

deriving instance (Hashable p, HasSigningScheme p) => Show (ImplVote p)

instance IsVote p (ImplVote p) where
  getVoter = vAuthor

  getConfidence = confidence

instance ( HasSigningScheme p
         , Signable p (SignedPayload (ImplVote p))
         ) => Signed p (ImplVote p) where

  type SignedPayload (ImplVote p) = ( Hash p (ImplementationData p)
                                    , Confidence
                                    , VKey p
                                    )

  signedPayload  ImplVote { vImplHash, confidence, vAuthor } =
    (vImplHash, confidence, vAuthor)

  signedBy ImplVote { vAuthor } = vAuthor

  payloadSignature ImplVote { vSig } = vSig

--------------------------------------------------------------------------------
-- ToCBOR instances
--------------------------------------------------------------------------------

instance ToCBOR ProtocolVersion where
  toCBOR (ProtocolVersion major minor) =
    encodeListLen 2 <> toCBOR major <> toCBOR minor

instance ToCBOR ParametersUpdate where
  toCBOR ParametersUpdate = encodeWord 0

instance ToCBOR ProtocolUpdateType where
  toCBOR ParametersOnly =
    encodeListLen 1 <> encodeWord 0
  toCBOR (SoftwareProtocol spuURL spuHash) =
    encodeListLen 3 <> encodeWord 0 <> toCBOR spuURL <> toCBOR spuHash

instance ( Typeable p
         , ToCBOR (VKey p)
         , ToCBOR (Hash p SIPData)
         , ToCBOR (ImplementationData p)
         ) => ToCBOR (Implementation p) where
  toCBOR (Implementation implAuthor implSalt implPayload)
    =  encodeListLen 3
    <> toCBOR implAuthor
    <> toCBOR implSalt
    <> toCBOR implPayload

instance ( Typeable p
         , ToCBOR (Hash p SIPData)
         , ToCBOR (UpdateType p)
         ) => ToCBOR (ImplementationData p) where
  toCBOR (ImplementationData implDataSIPHash implDataVPD  implType)
    =  encodeListLen 3
    <> toCBOR implDataSIPHash
    <> toCBOR implDataVPD
    <> toCBOR implType

instance ( Typeable p
         , ToCBOR (Hash p (ImplementationData p))
         ) => ToCBOR (UpdateType p) where
  toCBOR (Cancellation toCancel)
    =  encodeListLen 2
    <> encodeWord 0
    <> toCBOR toCancel
  toCBOR (Protocol puSupersedes puVersion puParameters puType)
    =  encodeListLen 5
    <> encodeWord 1
    <> toCBOR puSupersedes
    <> toCBOR puVersion
    <> toCBOR puParameters
    <> toCBOR puType
  toCBOR (Application auURL auImplHash)
    =  encodeListLen 2
    <> toCBOR auURL
    <> toCBOR auImplHash
