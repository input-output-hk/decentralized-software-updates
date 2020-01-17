{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Update.Ideation.Data where

import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import           Data.Typeable (Typeable, typeOf)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Cardano.Binary (ToCBOR (toCBOR), encodeInt, encodeListLen)
import           Data.AbstractSize (HasTypeReps)
import           Ledger.Core (SlotCount)

import           Cardano.Ledger.Spec.Classes.HasAuthor (HasAuthor, author)
import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)
import           Cardano.Ledger.Spec.Classes.HasSalt (HasSalt, salt)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     Signature, VKey)
import           Cardano.Ledger.Spec.State.Ballot (Ballot, Vote)
import qualified Cardano.Ledger.Spec.State.Ballot as Ballot
import           Cardano.Ledger.Spec.State.ProposalState (HasVotingPeriod,
                     getVotingPeriodDuration)
import           Cardano.Ledger.Spec.STS.Update.Data (Confidence, URL)
import           Cardano.Ledger.Spec.STS.Update.Data.Commit (Commit)

-- | Ideation signals.
--
-- TODO: consider renaming this to simply @Payload@ and using it qualified.
data IdeationPayload p
  = Submit (SIPCommit p) (SIP p)
  | Reveal (SIP p)
  | Vote (VoteForSIP p)
  deriving (Show, Generic)

isSubmit :: IdeationPayload p -> Bool
isSubmit (Submit {}) = True
isSubmit _ = False

isReveal :: IdeationPayload p -> Bool
isReveal (Reveal {}) = True
isReveal _ = False

data (VoteForSIP p) =
  VoteForSIP { votedsipHash :: !(SIPHash p)
               -- ^ SIP id that this ballot is for
             , confidence :: !Confidence
               -- ^ The ballot outcome
             , voter :: !(VKey p)
               -- ^ The voter
             , voterSig :: !(Signature p (SIPHash p, Confidence, VKey p))
             }
  deriving (Generic)

deriving instance (Hashable p, HasSigningScheme p) => Show (VoteForSIP p)

instance Vote (VoteForSIP p) (SIPHash p) p where
  candidate = votedsipHash

  confidence = confidence

  voter = voter

-- | Type alias to make it more convenient to instantiate 'Ballot' to
-- 'SIPHash'es as candidates.
type SIPBallot p = Ballot p (SIPHash p)

-- | Protocol version
--
-- NOTE: in we might want to add major, minor, and alt versions if necessary.
-- For now we can leave this abstract.
newtype ProtVer = ProtVer Word64
  deriving (Eq, Ord, Show)
  deriving stock (Generic)
  deriving newtype (HasTypeReps)

-- | Application version
newtype ApVer = ApVer Word64
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Num, ToCBOR)
  deriving anyclass (HasTypeReps)

-- | Consensus Protocol Parameter Name
data ParamName
  = BlockSizeMax
  | TxSizeMax
  | SlotSize
  | EpochSize
  deriving (Eq, Enum, Generic, Ord, Show, HasTypeReps)

-- | Flag to distinguish between `SIP`s that impact or not the
-- underlying consensus protocol
data ConcensusImpact = Impact | NoImpact
  deriving (Eq, Enum, Generic, Ord, Show, HasTypeReps)

-- | Metadata structure for SIP
data SIPMetadata =
  SIPMetadata
    { versionFrom :: !(ProtVer, ApVer)
      -- ^ The version the this SIP has been based on
    , versionTo :: !(ProtVer, ApVer)
      -- ^ the version after the SIP takes effect
    , impactsConsensus :: !ConcensusImpact
      -- ^ Flag to determine an impact on the underlying consensus protocol
    , impactsParameters :: !([ParamName])
      -- ^ List of protocol parameters impacted

    , votPeriodDuration :: !SlotCount
      -- ^ Voting Period duration for this SIP
    }
  deriving (Eq, Generic, Ord, Show, HasTypeReps)

instance HasVotingPeriod SIPMetadata where
  getVotingPeriodDuration = votPeriodDuration

-- | Contents of a SIP
data SIPData =
  SIPData
    {  url :: !URL
      -- ^ URL pointing at the server where the SIP is stored
    , metadata :: !SIPMetadata
      -- ^ SIP Metadata (only core metadata, the rest are on the server pointed
      -- by the url)
    }
  deriving (Eq, Generic, Ord, Show, HasTypeReps)

instance HasVotingPeriod SIPData where
  getVotingPeriodDuration = getVotingPeriodDuration . metadata

-- | Hash of the SIP contents (`SIPData`) also plays the role of a SIP
-- unique id
data SIPHash p = SIPHash (Hash p SIPData)
  deriving (Generic)

deriving instance Hashable p => Eq (SIPHash p)
deriving instance Hashable p => Ord (SIPHash p)
deriving instance Hashable p => Show (SIPHash p)

-- | System improvement proposal
data SIP p =
  SIP
    { sipHash :: SIPHash p
      -- ^ Hash of the SIP contents (`SIPData`) also plays the role of a SIP
      -- unique id
    , sipAuthor :: !(VKey p)
      -- ^ Who submitted the proposal.
    , sipSalt :: !Int
      -- ^ The salt used during the commit phase
    , sipPayload :: !SIPData
      -- ^ The actual contents of the SIP.
    }
  deriving (Generic)

deriving instance (HasSigningScheme p, Show (SIPHash p)) => Show (SIP p)
deriving instance (HasSigningScheme p, Eq (SIPHash p)) => Eq (SIP p)
instance ( Hashable p
         , HasHash p (VKey p)
         , HasSigningScheme p
         ) => Ord (SIP p) where
  sip0 <= sip1
    = (sipHash sip0, hash @p (author sip0), salt sip0, sipPayload sip0)
      <=
      (sipHash sip1, hash @p (author sip1), salt sip1, sipPayload sip1)

instance HasVotingPeriod (SIP p) where
  getVotingPeriodDuration = getVotingPeriodDuration . sipPayload

instance HasAuthor (SIP p) p where
  author = sipAuthor

instance HasSalt (SIP p) where
  salt = sipSalt

-- | The System improvement proposal at the commit phase
data SIPCommit p =
  SIPCommit
    { commit :: !(Commit p (SIP p))
      -- ^ A salted commitment (a hash) to the SIP id, the public key and the
      -- `hash` `SIP` (H(salt||pk||H(SIP)))
    , sipCommitAuthor :: !(VKey p)
      -- ^ Who submitted the proposal.
    , upSig :: !(Signature p (Commit p (SIP p)))
      -- ^ A signature on commit by the author public key
    }
  deriving (Generic)

deriving instance (Hashable p, HasSigningScheme p) => Show (SIPCommit p)

instance HasAuthor (SIPCommit p) p where
  author = sipCommitAuthor

--------------------------------------------------------------------------------
-- HasTypeReps instances
--------------------------------------------------------------------------------

deriving instance ( Typeable p
                  , HasTypeReps p
                  , HasTypeReps (SIP p)
                  , HasTypeReps (SIPHash p)
                  , HasTypeReps (SIPCommit p)
                  , HasTypeReps (VoteForSIP p)
                  ) => HasTypeReps (IdeationPayload p)

deriving instance ( Typeable p
                  , HasTypeReps (SIPHash p)
                  , HasTypeReps (VKey p)
                  ) => HasTypeReps (SIP p)

deriving instance ( Typeable p
                  , HasTypeReps p
                  , HasTypeReps (Signature p (Commit p (SIP p)))
                  , HasTypeReps (VKey p)
                  ) => HasTypeReps (SIPCommit p)


deriving instance ( Typeable p
                  , HasTypeReps (SIPHash p)
                  , HasTypeReps (VKey p)
                  , HasTypeReps (Signature p (SIPHash p, Confidence, VKey p))
                  ) => HasTypeReps (VoteForSIP p)

deriving instance ( Typeable p
                  , HasTypeReps (Hash p SIPData)
                  ) => HasTypeReps (SIPHash p)

instance (Typeable p, HasTypeReps (IdeationPayload p)) => Sized (IdeationPayload p) where
  costsList ideationPayload = [(typeOf ideationPayload, 10)]

--------------------------------------------------------------------------------
-- ToCBOR instances
--------------------------------------------------------------------------------

type SIPHasCBORRep p = (Typeable p, ToCBOR (Hash p SIPData))

instance (SIPHasCBORRep p, ToCBOR (VKey p)) => ToCBOR (SIP p) where
  toCBOR SIP { sipHash, sipAuthor, sipSalt, sipPayload }
    =  encodeListLen 4
    <> toCBOR sipHash
    <> toCBOR sipAuthor
    <> toCBOR sipSalt
    <> toCBOR sipPayload

instance (SIPHasCBORRep p) =>ToCBOR (SIPHash p) where
  toCBOR (SIPHash sipHash)
    =  encodeListLen 1
    <> toCBOR sipHash

instance ToCBOR SIPData where
  toCBOR SIPData { url, metadata }
    =  encodeListLen 2
    <> toCBOR url
    <> toCBOR metadata

instance ToCBOR SIPMetadata where
  toCBOR SIPMetadata { versionFrom
                     , versionTo
                     , impactsConsensus
                     , impactsParameters
                     , votPeriodDuration
                     }
    =  encodeListLen 5
    <> toCBOR versionFrom
    <> toCBOR versionTo
    <> toCBOR impactsConsensus
    <> toCBOR impactsParameters
    <> toCBOR votPeriodDuration

instance ToCBOR ParamName where
  toCBOR = encodeInt . fromEnum

instance ToCBOR ConcensusImpact where
  toCBOR = encodeInt . fromEnum

instance ToCBOR ProtVer where
  toCBOR (ProtVer version) = encodeListLen 1 <> toCBOR version
