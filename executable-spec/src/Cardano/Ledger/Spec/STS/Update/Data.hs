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
{-# LANGUAGE DeriveDataTypeable #-}

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
import           Ledger.Core (SlotCount, SlotCount (SlotCount))

import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     Signature, VKey)
-- import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)

-- data (VoteForSU p u) =
--   VoteForSU { votedSUHash :: !(SUHash p u)
--                -- ^ `SIP` id that this ballot is for
--              , confidenceSU :: !Confidence
--                -- ^ The ballot outcome
--              , voterSU :: !(VKey p)
--                -- ^ The voter
--              , voterSigSU :: !(Signature p (SUHash p u, Confidence, VKey p))
--              }
--   deriving (Generic)

--deriving instance (Hashable p, HasSigningScheme p) => Show (VoteForSU p u)

data (VoteForSIP p) =
-- TODO: type VoteForSIP p = VoteForSU p (SIP p)
  VoteForSIP { votedSIPHash :: !(SIPHash p)
               -- ^ `SIP` id that this ballot is for
             , confidenceSIP :: !Confidence
               -- ^ The ballot outcome
             , voterSIP :: !(VKey p)
               -- ^ The voter
             , voterSigSIP :: !(Signature p (SIPHash p, Confidence, VKey p))
             }
  deriving (Generic)

deriving instance (Hashable p, HasSigningScheme p) => Show (VoteForSIP p)

-- type VoteForUP p = VoteForSU p (UP p)

data (VoteForUP p) =
  VoteForUP { votedUPHash :: !(UPHash p)
               -- ^ `UP` id that this ballot is for
             , confidenceUP :: !Confidence
               -- ^ The ballot outcome
             , voterUP :: !(VKey p)
               -- ^ The voter
             , voterSigUP :: !(Signature p (UPHash p, Confidence, VKey p))
             }
  deriving (Generic)

deriving instance ( Hashable p
                  , HasSigningScheme p
                  , Show (UPHash p)
                  ) => Show (VoteForUP p)

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
  deriving (Eq, Generic, Ord, Show, HasTypeReps, Typeable)

-- | Metadata structure for `UP`
data UPMetadata p =
  UPMetadata
    { sipReference :: !(SIPHash p)
       -- ^ The `SIP` that this `UP` implements (or part of)
    , votPeriodDurationUP :: !SlotCount
      -- ^ Voting Period duration for this SIP
    }
  deriving (Eq, Generic, Ord, Show)

deriving instance (Typeable p) => Typeable (UPMetadata p)

-- | Content of a SU
-- m : metadata
-- data SUData m =
--   SUData
--     { urlSU :: !URL
--       -- ^ URL pointing at the server where the SU is stored
--     , metadataSU :: !m
--       -- ^ `SU` Metadata (only core metadata, the rest are on the server pointed
--       -- by the url)
--     }
--   deriving (Eq, Generic, Ord, Show, HasTypeReps)


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

-- | Contents of a UP
-- type UPData p = SUData (UPMetadata p)

data UPData p =
  UPData
    {  urlUP :: !URL
      -- ^ URL pointing at the server where the SIP is stored
    , metadataUP :: !(UPMetadata p)
      -- ^ SIP Metadata (only core metadata, the rest are on the server pointed
      -- by the url)
    }
  deriving (Eq, Generic, Ord, Show)

newtype URL = URL { getText :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR)

-- | Hash of the SU contents also plays the role of a SU
-- unique id
-- p: hashing and signing algorithm
-- u: type of software update
-- data SUHash p u = SUHash (Hash p (SUHasData p))
--   deriving (Generic)
-- data SUHash p u = SIPh (SIPHash p) | UPh (UPHash p)
--   deriving (Generic)

-- deriving instance ( Hashable p
--                   ) => Eq (SUHash p d)

-- deriving instance ( Hashable p
--                   ) => Ord (SUHash p d)

-- deriving instance ( Hashable p
--                   ) => Show (SUHash p d)

-- | Hash of the SIP contents (`SIPData`) also plays the role of a SIP
-- unique id
data SIPHash p = SIPHash (Hash p SIPData)
  deriving (Generic)

-- | Hash of the UP contents (`UPData`) also plays the role of a UP
-- unique id
data UPHash p = UPHash (Hash p (UPData p))
  deriving (Generic)

-- deriving instance ( Eq (Hash p (UPData p))
--                   ) => Eq (UPHash p)

-- deriving instance ( Ord (Hash p (UPData p))
--                   ) => Ord (UPHash p)

-- deriving instance ( Show (Hash p (UPData p))
--                   ) => Show (UPHash p)

deriving instance Hashable p => Eq (SIPHash p)
deriving instance Hashable p => Ord (SIPHash p)
deriving instance Hashable p => Show (SIPHash p)

-- | A Software Update
-- The 1st parameter specified the hashing ans signing algorithms
-- the 2nd specified the type of the SU (`SIP`, `UP`)
-- data SU p u = SUsip (SIP p) | SUup (UP p)
--  deriving (Generic)

-- deriving instance ( Hashable p
--                   , HasSigningScheme p
--                   ) => Eq (SU p d)

-- | A Software Update
-- p: hashing and signing algorithm used
-- d: contents (i.e.,data) of the software update
-- data SU p d =
--   SU
--     { hashSU :: SUHash p d
--       -- ^ Hash of the SU contents also plays the role of a SU
--       -- unique id
--     , authorSU :: !(VKey p)
--       -- ^ Who submitted the proposal.
--     , saltSU :: !Int
--       -- ^ The salt used during the commit phase
--     , payloadSU :: !d
--       -- ^ The actual contents of the SU.
--     }
--   deriving (Generic)

-- deriving instance ( Hashable p
--                   , Hashable d
--                   , Eq (VKey p)
--                   , Eq d
--                   ) => Eq (SU p d)

-- deriving instance ( Hashable p
--                   , Hashable d
--                   , Show (VKey p)
--                   , Show d
--                   ) => Show (SU p d)

-- | System Improvement Proposal (SIP)
data SIP p =
-- TODO: type SIP p = SU p SIPData
  SIP
    { hashSIP :: SIPHash p
      -- ^ Hash of the SIP contents (`SIPData`) also plays the role of a SIP
      -- unique id
    , authorSIP :: !(VKey p)
      -- ^ Who submitted the proposal.
    , saltSIP :: !Int
      -- ^ The salt used during the commit phase
    , payloadSIP :: !SIPData
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
    = (hashSIP sip0, hash @p (authorSIP sip0), saltSIP sip0, payloadSIP sip0)
      <=
      (hashSIP sip1, hash @p (authorSIP sip1), saltSIP sip1, payloadSIP sip1)


-- | Update Proposal (UP)
--type UP p = SU p (UPData p)

-- -- | Update Proposal
data UP p =
  UP
    { hashUP :: UPHash p
      -- ^ Hash of the UP contents (`UPData`) also plays the role of a UP
      -- unique id
    , authorUP :: !(VKey p)
      -- ^ Who submitted the proposal.
    , saltUP :: !Int
      -- ^ The salt used during the commit phase
    , payloadUP :: !(UPData p)
      -- ^ The actual contents of the UP.
    }
  deriving (Generic)

deriving instance ( Hashable p
                  , Eq (VKey p)
                  , Eq (UPHash p)
                  ) => Eq (UP p)

deriving instance ( Hashable p
                  , Show (VKey p)
                  , Show (UPHash p)
                  ) => Show (UP p)



-- | A Commitment for a Software Update
-- It is the `hash` $ (salt, sip_owner_pk,`hash` `SU`)
-- p: the hashing and signing algorithm
-- u: the type of the software update (`SIP` or `UP`)
newtype CommitSU p u =
  CommitSU
    { getCommitSU :: Hash p (Int, VKey p, Hash p u)
    }
  deriving stock (Generic)
-- data CommitSU p u = ComSIP (CommitSIP p) | ComUP (CommitUP p)

deriving instance (Hashable p) => Eq (CommitSU p u)
deriving instance Hashable p => Ord (CommitSU p u)
deriving instance (Hashable p) => Show (CommitSU p u)
deriving newtype instance ( Typeable p
                          , Typeable u
                          , ToCBOR (Hash p (Int, VKey p, Hash p u))
                          ) => (ToCBOR (CommitSU p u))


-- | A commitment data type for SIPs.
-- It is the `hash` $ (salt, sip_owner_pk,`hash` `SIP`)
newtype CommitSIP p =
-- TODO: type CommitSIP p = CommitSU p (SIP p)
  CommitSIP
    { getCommit :: Hash p (Int, VKey p, Hash p (SIP p))
    }
  deriving stock (Generic)

-- type CommitUP p = CommitSU p (UP p)

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

-- | The Software Update (`SU`) at the commit phase
-- p: the hashing and signing algorithm
-- u: the type of the software update (`SIP` or `UP`)
-- data SUCommit p u =
--   SUCommit
--     { commitSU :: !(CommitSU p u)
--       -- ^ A salted commitment (a hash) to the SU id, the public key and the
--       -- `hash` `SU` (H(salt||pk||H(SU)))
--     , authorSUcom :: !(VKey p)
--       -- ^ Who submitted the proposal.
--     , sigSUcom :: !(Signature p (CommitSU p u))
--       -- ^ A signature on commit by the author public key
--     }
--   deriving (Generic)

-- deriving instance (Hashable p, HasSigningScheme p) => Show (SUCommit p u)

-- | The System Improvement Proposal (`SIP`) at the commit phase
data SIPCommit p =
-- TODO: type SIPCommit p = SUCommit p (SIP p)
  SIPCommit
    { commitSIP :: !(CommitSIP p)
      -- ^ A salted commitment (a hash) to the SIP id, the public key and the
      -- `hash` `SIP` (H(salt||pk||H(SIP)))
    , authorSIPcom :: !(VKey p)
      -- ^ Who submitted the proposal.
    , sigSIP :: !(Signature p (CommitSIP p))
      -- ^ A signature on commit by the author public key
    }
  deriving (Generic)

deriving instance (Hashable p, HasSigningScheme p) => Show (SIPCommit p)

-- | The Update Proposal at the commit phase
-- type UPCommit p = SUCommit p (UP p)
data UPCommit p =
  UPCommit
    { commitUP :: !(CommitUP p)
      -- ^ A salted commitment (a hash) to the UP id, the public key and the
      -- `hash` `UP` (H(salt||pk||H(UP)))
    , authorUPcom :: !(VKey p)
      -- ^ Who submitted the proposal.
    , sigUP :: !(Signature p (CommitUP p))
      -- ^ A signature on commit by the author public key
    }
  deriving (Generic)

deriving instance (Hashable p, HasSigningScheme p) => Show (UPCommit p)

-- | Calculate a `Commit` from a `SIP`
calcCommit
  :: ( Hashable p
     , HasHash p (SIP p)
     , HasHash p (Int, VKey p, Hash p (SIP p))
     ) => SIP p -> CommitSIP p
calcCommit sip@SIP { saltSIP, authorSIP } =
  CommitSIP $ hash (saltSIP, authorSIP, hash sip)


-- | Calculate a `Commit` from a `UP`
calcCommitUP
  :: ( Hashable p
     , HasHash p (UP p)
     , HasHash p (Int, VKey p, Hash p (UP p))
     ) => UP p -> CommitUP p
calcCommitUP up@UP { saltUP, authorUP } =
  CommitUP $ hash (saltUP, authorUP, hash up)

--------------------------------------------------------------------------------
-- HasTypeReps instances
--------------------------------------------------------------------------------

-- | This instance returns one 'Char' per-each character in the URL.
instance HasTypeReps URL where
  typeReps (URL text)
    = Seq.fromList
    $ typeOf (undefined :: URL)
      : replicate (T.length text) (typeOf (undefined :: Char))

-- deriving instance ( Typeable p
--                   , Typeable d
--                   , HasTypeReps (SUHash p d)
--                   , HasTypeReps (VKey p)
--                   , HasTypeReps d
--                   ) => HasTypeReps (SU p d)

deriving instance ( Typeable p
                  , HasTypeReps (SIPHash p)
                  , HasTypeReps (VKey p)
                  ) => HasTypeReps (SIP p)

deriving instance ( Typeable p
                  , HasTypeReps (Hash p SIPData)
                  , HasTypeReps (UPHash p)
                  , HasTypeReps (VKey p)
                  ) => HasTypeReps (UP p)

-- | A commit is basically wrapping the hash of some salt, owner verification
-- key, and SU. The size of the hash is determined by the type of hash
-- algorithm
-- instance HasTypeReps p => HasTypeReps (CommitSU p u) where
--   typeReps _ = typeReps (undefined :: p)

instance HasTypeReps p => HasTypeReps (CommitSIP p) where
  typeReps _ = typeReps (undefined :: p)

instance HasTypeReps p => HasTypeReps (CommitUP p) where
  typeReps _ = typeReps (undefined :: p)

-- instance ( Typeable p
--          , Typeable u
--          ) => HasTypeReps (Hash p (CommitSU p u)) where
--   typeReps commitHash = Seq.singleton (typeOf commitHash)

instance Typeable p => HasTypeReps (Hash p (CommitSIP p)) where
  typeReps commitHash = Seq.singleton (typeOf commitHash)

instance Typeable p => HasTypeReps (Hash p (CommitUP p)) where
  typeReps commitHash = Seq.singleton (typeOf commitHash)

deriving instance ( Typeable p
                  , HasTypeReps p
                  , HasTypeReps (Signature p (CommitSIP p))
                  , HasTypeReps (VKey p)
                  ) => HasTypeReps (SIPCommit p)

-- deriving instance ( Typeable p
--                   , Typeable u
--                   , HasTypeReps p
--                   , HasTypeReps u
--                   , HasTypeReps (Signature p (CommitSU p u))
--                   , HasTypeReps (VKey p)
--                   ) => HasTypeReps (SUCommit p u)

deriving instance ( Typeable p
                  , HasTypeReps p
                  , HasTypeReps (Signature p (CommitUP p))
                  , HasTypeReps (VKey p)
                  ) => HasTypeReps (UPCommit p)

deriving instance ( Typeable p
                  , HasTypeReps (SIPHash p)
                  , HasTypeReps (VKey p)
                  , HasTypeReps (Signature p (SIPHash p, Confidence, VKey p))
                  ) => HasTypeReps (VoteForSIP p)

-- deriving instance ( Typeable p
--                   , Typeable u
--                   , HasTypeReps (SUHash p u)
--                   , HasTypeReps (VKey p)
--                   , HasTypeReps (Signature p (SUHash p u, Confidence, VKey p))
--                   ) => HasTypeReps (VoteForSU p u)

deriving instance ( Typeable p
                  , HasTypeReps (UPHash p)
                  , HasTypeReps (VKey p)
                  , HasTypeReps (Signature p (SIPHash p, Confidence, VKey p))
                  , HasTypeReps (Signature p (UPHash p, Confidence, VKey p))
                  ) => HasTypeReps (VoteForUP p)

-- deriving instance ( Typeable p
--                   , Typeable d
--                   , HasTypeReps (Hash p d)
--                   ) => HasTypeReps (SUHash p d)

deriving instance ( Typeable p
                  , HasTypeReps (Hash p SIPData)
                  ) => HasTypeReps (SIPHash p)

deriving instance ( Typeable p
                  , HasTypeReps (Hash p (UPData p))
                  ) => HasTypeReps (UPHash p)

deriving instance ( Typeable p
                  , HasTypeReps (Hash p SIPData)
                  ) => HasTypeReps (UPData p)

deriving instance ( Typeable p
                  , HasTypeReps (Hash p SIPData)
                  ) => HasTypeReps (UPMetadata p)

--------------------------------------------------------------------------------
-- ToCBOR instances
--------------------------------------------------------------------------------

-- type SUHasCBORRep p d = (Typeable p, ToCBOR (Hash p d))

type SIPHasCBORRep p = (Typeable p, ToCBOR (Hash p SIPData))

type UPHasCBORRep p = (Typeable p, ToCBOR (Hash p (UPData p)))

-- instance ( Typeable d
--          , ToCBOR d
--          , SUHasCBORRep p d
--          , ToCBOR (VKey p)
--          ) => ToCBOR (SU p d) where
--   toCBOR SU { hashSU, authorSU, saltSU , payloadSU }
--     =  encodeListLen 4
--     <> toCBOR hashSU
--     <> toCBOR authorSU
--     <> toCBOR saltSU
--     <> toCBOR payloadSU

instance ( SIPHasCBORRep p
         , ToCBOR (VKey p)
         ) => ToCBOR (SIP p) where
  toCBOR SIP { hashSIP, authorSIP, saltSIP , payloadSIP }
    =  encodeListLen 4
    <> toCBOR hashSIP
    <> toCBOR authorSIP
    <> toCBOR saltSIP
    <> toCBOR payloadSIP

instance ( UPHasCBORRep p
         , ToCBOR (VKey p)
         , ToCBOR (Hash p SIPData)
         ) => ToCBOR (UP p) where
  toCBOR UP { hashUP, authorUP, saltUP, payloadUP }
    =  encodeListLen 4
    <> toCBOR hashUP
    <> toCBOR authorUP
    <> toCBOR saltUP
    <> toCBOR payloadUP

-- instance (Typeable d, SUHasCBORRep p d) =>ToCBOR (SUHash p d) where
--   toCBOR (SUHash suHash)
--     =  encodeListLen 1
--     <> toCBOR suHash

instance (SIPHasCBORRep p) =>ToCBOR (SIPHash p) where
  toCBOR (SIPHash sipHash)
    =  encodeListLen 1
    <> toCBOR sipHash

instance (UPHasCBORRep p) =>ToCBOR (UPHash p) where
  toCBOR (UPHash upHash)
    =  encodeListLen 1
    <> toCBOR upHash

-- instance (Typeable m, ToCBOR m) => ToCBOR (SUData m) where
--   toCBOR SUData { urlSU, metadataSU }
--     =  encodeListLen 2
--     <> toCBOR urlSU
--     <> toCBOR metadataSU

instance ToCBOR SIPData where
  toCBOR SIPData { url, metadata }
    =  encodeListLen 2
    <> toCBOR url
    <> toCBOR metadata

instance ( Typeable p
         , ToCBOR (Hash p SIPData)
         ) => ToCBOR (UPData p) where
  toCBOR UPData { urlUP, metadataUP }
    =  encodeListLen 2
    <> toCBOR urlUP
    <> toCBOR metadataUP

instance ToCBOR SIPMetadata where
  toCBOR SIPMetadata { versionFrom, versionTo
                     , impactsConsensus, impactsParameters, votPeriodDuration }
    =  encodeListLen 5
    <> toCBOR versionFrom
    <> toCBOR versionTo
    <> toCBOR impactsConsensus
    <> toCBOR impactsParameters
    <> toCBOR votPeriodDuration

instance ( Typeable p
         , ToCBOR (Hash p SIPData)
         ) => ToCBOR (UPMetadata p) where
  toCBOR UPMetadata { sipReference, votPeriodDurationUP }
    =  encodeListLen 2
    <> toCBOR sipReference
    <> toCBOR votPeriodDurationUP

instance ToCBOR SlotCount where
  toCBOR (SlotCount sc) =  encodeListLen 1 <> toCBOR sc

instance ToCBOR ParamName where
  toCBOR = encodeInt . fromEnum

instance ToCBOR ConcensusImpact where
  toCBOR = encodeInt . fromEnum

instance ToCBOR ProtVer where
  toCBOR (ProtVer version) = encodeListLen 1 <> toCBOR version

instance ToCBOR Confidence where
  toCBOR = encodeInt . fromEnum
