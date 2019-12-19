{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Update.Data where

import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable, typeOf)
import           Data.Word (Word64, Word8)
import           GHC.Generics (Generic)
import           System.Random (Random)

import           Cardano.Binary (ToCBOR (toCBOR), encodeInt, encodeListLen)

import           Data.AbstractSize (HasTypeReps, typeReps)
import           Ledger.Core (SlotCount)

import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     Signature, VKey)
import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)


data ImplementationPayload = ImplementationPayload
  deriving (Eq, Show, Generic, HasTypeReps)

-- | Ideation signals.
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

-- | Vote Confidence with a 3-valued logic
data Confidence = For | Against | Abstain
  deriving (Eq, Ord, Show, Enum, Generic, HasTypeReps)

-- | Records the voting result for a specific software update (SIP/UP)
data VotingResult =
  VotingResult { stakeInFavor :: !Stake
               , stakeAgainst :: !Stake
               , stakeAbstain :: !Stake
               , rvNoQuorum :: Word8
               -- ^ No quorum revoting : how many times revoting has taken place
               -- due to a no quorum result
               --
               -- TODO: this should be a newtype
              , rvNoMajority :: Word8
               -- ^ No majority revoting : how many times revoting has taken
               -- place due to a no majority result
               --
               -- TODO: this should be a newtype
               }
  deriving (Eq, Ord, Show)

addVote :: Stake -> Confidence -> VotingResult -> VotingResult
addVote
  stake
  confidence
  votingResult@VotingResult { stakeInFavor, stakeAgainst, stakeAbstain }
  =
  case confidence of
    For -> votingResult { stakeInFavor = stakeInFavor + stake}
    Against -> votingResult { stakeAgainst = stakeAgainst + stake}
    Abstain -> votingResult { stakeAbstain = stakeAbstain + stake}

data TallyOutcome = Approved | Rejected | NoQuorum | NoMajority | Expired
  deriving (Eq, Ord, Show)

-- | Returns the stake percent as a rounded value
-- in the range [0,100]
stakePercentRound
 :: Stake
 -> Stake -- ^ Stake total
 -> Word8
stakePercentRound st totSt =
  round @Float $ fromIntegral st / fromIntegral totSt * 100

-- | Stake
newtype Stake = Stake { getStake :: Word64 }
 deriving newtype (Eq, Ord, Show, Enum, Num, Integral, Real, Random)

-- | Voting Period status values
data VPStatus = VPOpen | VPClosed
  deriving (Eq, Ord, Show)

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

-- | Metadata structure for `SIP`
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

-- | Metadata structure for `UP`
data UPMetadata =
  UPMetadata
    { versionFromUP :: !(ProtVer, ApVer)
      -- ^ The version the this SIP has been based on
    , versionToUP :: !(ProtVer, ApVer)
      -- ^ the version after the SIP takes effect
    , impactsConsensusUP :: !ConcensusImpact
      -- ^ Flag to determine an impact on the underlying consensus protocol
    , impactsParametersUP :: !([ParamName])
      -- ^ List of protocol parameters impacted

    , votPeriodDurationUP :: !SlotCount
      -- ^ Voting Period duration for this SIP
    }
  deriving (Eq, Generic, Ord, Show, HasTypeReps)

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

-- Contents of a UP
data UPData =
  UPData
    {  urlUP :: !URL
      -- ^ URL pointing at the server where the SIP is stored
    , metadataUP :: !UPMetadata
      -- ^ SIP Metadata (only core metadata, the rest are on the server pointed
      -- by the url)
    }
  deriving (Eq, Generic, Ord, Show, HasTypeReps)


newtype URL = URL { getText :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR)

-- | Hash of the SU contents also plays the role of a SU
-- unique id
data SUHash p u = SIPh (SIPHash p) | UPh (UPHash p)
  deriving (Generic)

deriving instance ( Hashable p
                  ) => Eq (SUHash p u)

deriving instance ( Hashable p
                  ) => Ord (SUHash p u)

deriving instance ( Hashable p
                  ) => Show (SUHash p u)

-- | Hash of the SIP contents (`SIPData`) also plays the role of a SIP
-- unique id
data SIPHash p = SIPHash (Hash p SIPData)
  deriving (Generic)

-- | Hash of the UP contents (`UPData`) also plays the role of a UP
-- unique id
data UPHash p = UPHash (Hash p UPData)
  deriving (Generic)

deriving instance ( Eq (Hash p UPData)
                  ) => Eq (UPHash p)

deriving instance ( Ord (Hash p UPData)
                  ) => Ord (UPHash p)

deriving instance ( Show (Hash p UPData)
                  ) => Show (UPHash p)

deriving instance Hashable p => Eq (SIPHash p)
deriving instance Hashable p => Ord (SIPHash p)
deriving instance Hashable p => Show (SIPHash p)

-- | A Software Update
-- The 1st parameter specified the hashing ans signing algorithms
-- the 2nd specified the type of the SU (`SIP`, `UP`)
data SU p u = SUsip (SIP p) | SUup (UP p)
 deriving (Generic)

deriving instance ( Hashable p
                  , HasSigningScheme p
                  ) => Eq (SU p u)

deriving instance ( Hashable p
                  , HasSigningScheme p
                  ) => Show (SU p u)

-- | System improvement proposal
data SIP p =
  SIP
    { sipHash :: SIPHash p
      -- ^ Hash of the SIP contents (`SIPData`) also plays the role of a SIP
      -- unique id
    , author :: !(VKey p)
      -- ^ Who submitted the proposal.
    , salt :: !Int
      -- ^ The salt used during the commit phase
    , sipPayload :: !SIPData
      -- ^ The actual contents of the SIP.
    }
  deriving (Generic)

-- | Update Proposal
data UP p =
  UP
    { upHash :: UPHash p
      -- ^ Hash of the UP contents (`UPData`) also plays the role of a UP
      -- unique id
    , upauthor :: !(VKey p)
      -- ^ Who submitted the proposal.
    , sipReference :: SIPHash p
      -- ^ The `SIP` that this `UP` implements (or part of)
    , upsalt :: !Int
      -- ^ The salt used during the commit phase
    , upPayload :: !UPData
      -- ^ The actual contents of the UP.
    }
  deriving (Generic)

deriving instance ( Hashable p
                  , Eq (VKey p)
                  ) => Eq (UP p)

deriving instance ( Hashable p
                  , Show (VKey p)
                  ) => Show (UP p)

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

-- | A Commitment for a Software Update
data CommitSU p u = ComSIP (CommitSIP p) | ComUP (CommitUP p)

deriving instance (Hashable p) => Eq (CommitSU p u)
deriving instance Hashable p => Ord (CommitSU p u)
deriving instance (Hashable p) => Show (CommitSU p u)


-- | A commitment data type for SIPs.
-- It is the `hash` $ (salt, sip_owner_pk,`hash` `SIP`)
newtype CommitSIP p =
  CommitSIP
    { getCommit :: Hash p (Int, VKey p, Hash p (SIP p))
    }
  deriving stock (Generic)

-- | A commitment data type for UPs.
-- It is the `hash` $ (salt, sip_owner_pk,`hash` `SIP`)
newtype CommitUP p =
  CommitUP
    { getCommitUP :: Hash p (Int, VKey p, Hash p (UP p))
    }
  deriving stock (Generic)

deriving instance Hashable p => Eq (CommitSIP p)
deriving instance Hashable p => Ord (CommitSIP p)
deriving instance Hashable p => Show (CommitSIP p)
deriving newtype instance ( Typeable p
                          , ToCBOR (Hash p (Int, VKey p, Hash p (SIP p)))
                          ) => (ToCBOR (CommitSIP p))


deriving instance Hashable p => Eq (CommitUP p)
deriving instance Hashable p => Ord (CommitUP p)
deriving instance Hashable p => Show (CommitUP p)
deriving newtype instance ( Typeable p
                          , ToCBOR (Hash p (Int, VKey p, Hash p (UP p)))
                          ) => (ToCBOR (CommitUP p))

-- | The System improvement proposal at the commit phase
data SIPCommit p =
  SIPCommit
    { commit :: !(CommitSIP p)
      -- ^ A salted commitment (a hash) to the SIP id, the public key and the
      -- `hash` `SIP` (H(salt||pk||H(SIP)))
    ,  _author :: !(VKey p)
      -- ^ Who submitted the proposal.
    , upSig :: !(Signature p (CommitSIP p))
      -- ^ A signature on commit by the author public key
    }
  deriving (Generic)

deriving instance (Hashable p, HasSigningScheme p) => Show (SIPCommit p)

-- | Calculate a `Commit` from a `SIP`
calcCommit
  :: ( Hashable p
     , HasHash p (SIP p)
     , HasHash p (Int, VKey p, Hash p (SIP p))
     ) => SIP p -> CommitSIP p
calcCommit sip@SIP { salt, author } =
  CommitSIP $ hash (salt, author, hash sip)

--------------------------------------------------------------------------------
-- HasTypeReps instances
--------------------------------------------------------------------------------

-- | This instance returns one 'Char' per-each character in the URL.
instance HasTypeReps URL where
  typeReps (URL text)
    = Seq.fromList
    $ typeOf (undefined :: URL)
      : replicate (T.length text) (typeOf (undefined :: Char))

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

-- | A commit is basically wrapping the hash of some salt, owner verification
-- key, and SIP. The size of the hash is determined by the type of hash
-- algorithm
instance HasTypeReps p => HasTypeReps (CommitSIP p) where
  typeReps _ = typeReps (undefined :: p)

instance Typeable p => HasTypeReps (Hash p (CommitSIP p)) where
  typeReps commitHash = Seq.singleton (typeOf commitHash)

deriving instance ( Typeable p
                  , HasTypeReps p
                  , HasTypeReps (Signature p (CommitSIP p))
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

--------------------------------------------------------------------------------
-- Sized instances
--------------------------------------------------------------------------------

instance Sized ImplementationPayload where
  costsList implementationPayload = [(typeOf implementationPayload, 10)]

instance (Typeable p, HasTypeReps (IdeationPayload p)) => Sized (IdeationPayload p) where
  costsList ideationPayload = [(typeOf ideationPayload, 10)]

--------------------------------------------------------------------------------
-- ToCBOR instances
--------------------------------------------------------------------------------

type SIPHasCBORRep p = (Typeable p, ToCBOR (Hash p SIPData))

instance (SIPHasCBORRep p, ToCBOR (VKey p)) => ToCBOR (SIP p) where
  toCBOR SIP { sipHash, author, salt, sipPayload }
    =  encodeListLen 4
    <> toCBOR sipHash
    <> toCBOR author
    <> toCBOR salt
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
  toCBOR SIPMetadata { versionFrom, versionTo, impactsConsensus, impactsParameters }
    =  encodeListLen 4
    <> toCBOR versionFrom
    <> toCBOR versionTo
    <> toCBOR impactsConsensus
    <> toCBOR impactsParameters

instance ToCBOR ParamName where
  toCBOR = encodeInt . fromEnum

instance ToCBOR ConcensusImpact where
  toCBOR = encodeInt . fromEnum

instance ToCBOR ProtVer where
  toCBOR (ProtVer version) = encodeListLen 1 <> toCBOR version

instance ToCBOR Confidence where
  toCBOR = encodeInt . fromEnum
