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

import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable, typeOf)
import           Data.Word (Word64, Word8)
import           GHC.Generics (Generic)

import           Cardano.Binary (ToCBOR (toCBOR), encodeInt, encodeListLen)
import           Cardano.Crypto.DSIGN.Class (SignedDSIGN)
import           Cardano.Crypto.Hash (Hash, HashAlgorithm, hash)

import           Data.AbstractSize (HasTypeReps, typeReps)
import           Ledger.Core (SlotCount (SlotCount))
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import           Cardano.Ledger.Spec.STS.Update.Definitions (vThreshold)


data ImplementationPayload = ImplementationPayload
  deriving (Eq, Show, Generic, HasTypeReps)

-- | Ideation signals.
data IdeationPayload hashAlgo dsignAlgo
  = Submit (SIPCommit hashAlgo dsignAlgo) (SIP hashAlgo)
  | Reveal (SIP hashAlgo)
  | Vote (BallotSIP hashAlgo)
  deriving (Eq, Show, Generic)

isSubmit :: IdeationPayload hashAlgo dsignAlgo -> Bool
isSubmit (Submit {}) = True
isSubmit _ = False

isReveal :: IdeationPayload hashAlgo dsignAlgo -> Bool
isReveal (Reveal {}) = True
isReveal _ = False

-- | This is the ballot for a SIP
data (BallotSIP hashAlgo) =
  BallotSIP { votedsipHash :: !(SIPHash hashAlgo)
              -- ^ SIP id that this ballot is for
            , confidence :: !Confidence
              -- ^ the ballot outcome
            , voter :: !Core.VKey
              -- ^ the voter
            , voterSig :: !(Core.Sig ( SIPHash hashAlgo
                                     , Confidence
                                     , Core.VKey
                                     )
                           )
            }
  deriving (Eq, Ord, Show, Generic)

-- | Vote Confidence with a 3-valued logic
data Confidence = For | Against | Abstain
  deriving (Eq, Ord, Show, Generic, HasTypeReps)

-- | Records the voting result for a specific software update (SIP/UP)
data VotingResult =
  VotingResult { stakeInFavor :: !Stake
               , stakeAgainst :: !Stake
               , stakeAbstain :: !Stake
               , rvNoQuorum :: Word8
                 -- ^ No quorum revoting : how many times
                 -- revoting has taken place due to a no quorum result
              , rvNoMajority :: Word8
                 -- ^ No majority revoting : how many times
                 -- revoting has taken place due to a no majority result
               }
  deriving (Eq, Ord, Show)

data TallyOutcome = Approved | Rejected | NoQuorum | NoMajority | Expired
  deriving (Eq, Ord, Show)

-- | Return the outcome of the tally based on a  `VotingResult` and
-- a stake distribution.
tallyOutcome
  :: VotingResult
  -> Map Core.VKey Stake
  -> Word8  -- ^ max number of revoting for No Quorum
  -> Word8  -- ^ max number of revoting for No Majority
  -> Float  -- ^ adversary stake ratio
  -> TallyOutcome
tallyOutcome vres sDist pNoQ pNoM r_a =
  if stakePercentRound (stakeInFavor vres) (totalStake sDist)
     > vThreshold r_a
    then
      Approved
    else
      if stakePercentRound (stakeAgainst vres) (totalStake sDist)
         > vThreshold r_a
        then
          Rejected
        else
          if stakePercentRound (stakeAbstain vres) (totalStake sDist)
             > vThreshold r_a
             && rvNoQuorum vres <= pNoQ
            then
              NoQuorum
            else
              if stakePercentRound (stakeInFavor vres) (totalStake sDist)
                 <= vThreshold r_a
                 &&
                 stakePercentRound (stakeAgainst vres) (totalStake sDist)
                 <= vThreshold r_a
                 &&
                 stakePercentRound (stakeAbstain vres) (totalStake sDist)
                 <= vThreshold r_a
                 && rvNoMajority vres <= pNoM
                then
                  NoMajority
                else
                  Expired

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
 deriving newtype (Eq, Ord, Show, Enum, Num, Integral, Real)

-- | Returns the total stake from a stake distribution
totalStake :: (Map Core.VKey Stake) -> Stake
totalStake m =
  Map.foldr' (\stk tot -> tot + stk) (Stake 0) m

-- | Duration of a Voting Period
data VPDuration = VPMin | VPMedium | VPLarge
  deriving (Eq, Ord, Show, Generic, HasTypeReps)

-- | Voting Period status values
data VPStatus = VPOpen | VPClosed
  deriving (Eq, Ord, Show)

-- | Includes logic to translate a voting period duration
-- to number of slots
vpDurationToSlotCnt :: VPDuration -> SlotCount
vpDurationToSlotCnt  d =
  case d of
    VPMin -> SlotCount 20
    VPMedium -> SlotCount 50
    VPLarge -> SlotCount 100

-- | Return in how many slots the voting period
-- of the specific `SIP` will end, based on
-- the voting period duration recorded
-- in its metadata `SIPMetadata`
-- The 2nd argument plays the role of a "SIP database"
votPeriodEnd
  :: (SIPHash hashAlgo)
  -> Map (SIPHash hashAlgo) (SIP hashAlgo)
  -> SlotCount
votPeriodEnd siphash sipdb =  vpDurationToSlotCnt
                           $ votPeriodDuration
                           . metadata
                           . sipPayload
                           $ (sipdb!siphash)

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
    , votPeriodDuration :: !VPDuration
      -- Voting Period duration for this SIP
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

newtype URL = URL { getText :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR)

-- | Hash of the SIP contents (`SIPData`) also plays the role of a SIP
-- unique id
data SIPHash hashAlgo = SIPHash (Hash hashAlgo SIPData)
  deriving (Eq, Generic, Ord, Show)

-- | System improvement proposal
data SIP hashAlgo =
  SIP
    { sipHash :: SIPHash hashAlgo
      -- ^ Hash of the SIP contents (`SIPData`) also plays the role of a SIP
      -- unique id
    , author :: !Core.VKey
      -- ^ Who submitted the proposal.
    , salt :: !Int
      -- ^ The salt used during the commit phase
    , sipPayload :: !SIPData
      -- ^ The actual contents of the SIP.
    }
  deriving (Eq, Generic, Ord, Show)

-- | A commitment data type.
-- It is the `hash` $ (salt, sip_owner_pk,`hash` `SIP`)
newtype Commit hashAlgo =
  Commit
    { getCommit
      :: Hash hashAlgo
           ( Int
           , Core.VKey
           , Hash hashAlgo (SIP hashAlgo)
           )
    }
  deriving stock (Generic)
  deriving (Show, Eq, Ord)

-- | The System improvement proposal at the commit phase
data SIPCommit hashAlgo dsignAlgo =
  SIPCommit
    { commit :: !(Commit hashAlgo)
      -- ^ A salted commitment (a hash) to the SIP id, the public key and the
      -- `hash` `SIP` (H(salt||pk||H(SIP)))
    , _author :: !Core.VKey
      -- ^ Who submitted the proposal.
    , upSig :: !(SignedDSIGN dsignAlgo (Commit hashAlgo))
      -- ^ A signature on commit by the author public key
    }
  deriving (Eq, Show, Generic)

-- | Calculate a `Commit` from a `SIP`
calcCommit :: HashAlgorithm hashAlgo => SIP hashAlgo -> Commit hashAlgo
calcCommit sip@SIP { salt, author } =
  Commit $ hash (salt, author, hash sip)

--------------------------------------------------------------------------------
-- HasTypeReps instances
--------------------------------------------------------------------------------

-- | This instance returns one 'Char' per-each character in the URL.
instance HasTypeReps URL where
  typeReps (URL text)
    = Seq.fromList
    $ typeOf (undefined :: URL)
      : replicate (T.length text) (typeOf (undefined :: Char))

deriving instance ( Typeable hashAlgo
                  , Typeable dsignAlgo
                  , HasTypeReps (SIP hashAlgo)
                  , HasTypeReps hashAlgo
                  , HasTypeReps (Hash hashAlgo SIPData)
                  , HasTypeReps (SignedDSIGN dsignAlgo (Commit hashAlgo))
                  ) => HasTypeReps (IdeationPayload hashAlgo dsignAlgo)

deriving instance ( HasTypeReps (Hash hashAlgo SIPData)
                  , Typeable hashAlgo
                  , HasTypeReps hashAlgo
                  ) => HasTypeReps (SIP hashAlgo)

-- | A commit is basically wrapping the hash of some salt, owner verification
-- key, and SIP. The size of the hash is determined by the type of hash
-- algorithm
instance HasTypeReps hashAlgo => HasTypeReps (Commit hashAlgo) where
  typeReps _ = typeReps (undefined :: hashAlgo)

instance Typeable hashAlgo => HasTypeReps (Hash hashAlgo (Commit hashAlgo)) where
  typeReps commitHash = Seq.singleton (typeOf commitHash)

deriving instance ( Typeable hashAlgo
                  , Typeable dsignAlgo
                  , HasTypeReps hashAlgo
                  , HasTypeReps (SignedDSIGN dsignAlgo (Commit hashAlgo))
                  ) => HasTypeReps (SIPCommit hashAlgo dsignAlgo)


deriving instance ( Typeable hashAlgo
                  , HasTypeReps (Hash hashAlgo SIPData)
                  , HasTypeReps hashAlgo
                  ) => HasTypeReps (BallotSIP hashAlgo)

--------------------------------------------------------------------------------
-- Sized instances
--------------------------------------------------------------------------------

instance Sized ImplementationPayload where
  costsList implementationPayload = [(typeOf implementationPayload, 10)]

instance ( Typeable hashAlgo
         , Typeable dsignAlgo
         , HasTypeReps (Hash hashAlgo SIPData)
         , HasTypeReps hashAlgo
         , HasTypeReps (SignedDSIGN dsignAlgo (Commit hashAlgo))
         ) => Sized (IdeationPayload hashAlgo dsignAlgo) where
  costsList ideationPayload = [(typeOf ideationPayload, 10)]

--------------------------------------------------------------------------------
-- ToCBOR instances
--------------------------------------------------------------------------------

instance (HashAlgorithm hashAlgo) => ToCBOR (SIP hashAlgo) where
  toCBOR SIP { sipHash, author, salt, sipPayload }
    =  encodeListLen 4
    <> toCBOR sipHash
    <> toCBOR author
    <> toCBOR salt
    <> toCBOR sipPayload

instance (HashAlgorithm hashAlgo) => ToCBOR (SIPHash hashAlgo) where
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

deriving instance ( Typeable hashAlgo
                  , HasTypeReps hashAlgo
                  , HasTypeReps (Hash hashAlgo SIPData)
                  ) => HasTypeReps (SIPHash hashAlgo)
