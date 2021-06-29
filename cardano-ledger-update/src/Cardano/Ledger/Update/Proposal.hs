{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Update.Proposal where

import           Data.Typeable (Typeable)
import           Data.Kind (Type)
import           Data.Maybe (isJust)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Control.DeepSeq (NFData)
import           Data.Aeson (ToJSON, FromJSON)
import           Data.List (find)

import Cardano.Binary
  ( FromCBOR(fromCBOR)
  , ToCBOR(toCBOR)
  , decodeInt
  , decodeWord
  , encodeInt
  , encodeWord
  , encodeListLen
  , decodeListLenOf
  )
import           Cardano.Slotting.Slot (SlotNo)

-- | Data for which a commit can be computed.
class ( Eq (Commit d)
      , Ord (Commit d)
      , Show (Commit d) ) => Commitable d where
  type Commit d :: Type
  commit :: d -> Commit d

-- | Data for which an ID can be computed.
class ( Eq (Id p)
      , Ord (Id p)
      , Show (Id p) ) => Identifiable p where
  data Id p :: Type
  _id :: p -> Id p

-- | Signed data.
class Signed d where
  signatureVerifies :: d -> Bool

class ( Commitable (Revelation proposal)
      , Show (Commit (Revelation proposal))
      , Ord (Commit (Revelation proposal))

      , Signed (Submission proposal)
      , Signed (Vote proposal)

      , Show (Submission proposal)
      , Show (Revelation proposal)
      , Show proposal
      , Show (Vote proposal)

      -- Data we need to be able to identify in the system:
      , Identifiable (proposal)
      , Identifiable (Voter proposal)
      ) => Proposal proposal where

  data Submission proposal :: Type
  data Revelation proposal :: Type
  revelationCommit :: Submission proposal -> Commit (Revelation proposal)
  proposal :: Revelation proposal -> proposal

  votingPeriodDuration :: proposal -> SlotNo
  data Vote proposal :: Type
  data Voter proposal :: Type
  voter :: Vote proposal -> Id (Voter proposal)
  candidate :: Vote proposal -> Id proposal
  confidence :: Vote proposal -> Confidence

type VoterId p = Id (Voter p)

-- | Proposal payload
data Payload proposal
  = Submit (Submission proposal)
  | Reveal (Revelation proposal)
  | Cast   (Vote proposal)

deriving instance Proposal proposal => Show (Payload proposal)

data Confidence = For | Against | Abstain
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, NoThunks, NFData, ToJSON, FromJSON)

confidenceEncoding :: [(Int, Confidence)]
confidenceEncoding = [ (0, For)
                     , (1, Against)
                     , (2, Abstain)
                     ]

instance ToCBOR Confidence where
  toCBOR c =
    case find ((==c) . snd) confidenceEncoding of
      Nothing     -> error $ "Confidence " <> show c <> " is not in the encoding map"
      Just (i, _) -> encodeInt i

instance FromCBOR Confidence where
  fromCBOR = do
    i <- decodeInt
    case lookup i confidenceEncoding of
      Nothing -> fail $  "Decoded integer value '" <> show i
                      <> "' is an invalid encoding of a value of type 'Confidence'"
      Just r  -> return $! r

--------------------------------------------------------------------------------
-- Implementation proposals
--------------------------------------------------------------------------------

-- | An implementation is associated to a predecessor proposal. The type of the
-- implementation uniquely identifies the type of its predecessor.
class ( Proposal impl
      , Activable (Protocol impl)
      , Show (NonProtocol impl)
      , Identifiable (NonProtocol impl)

      , Identifiable sip
      ) => Implementation sip impl | impl -> sip where

  -- | An implementation must refer to a sip proposal, which has a
  -- different type.
  preProposalId :: impl -> Id sip

  implementationType :: impl -> ImplementationType impl
  -- | Type of protocols this implementation type implements.
  data Protocol impl :: Type
  -- | Type of non-protocol related updates this implementation type implements.
  data NonProtocol impl :: Type

-- | Extract the proposed protocol update, if any.
proposedProtocolUpdate
  :: Implementation sip impl => impl -> Maybe (Protocol impl)
proposedProtocolUpdate impl =
  case implementationType impl of
    Protocol protocol -> Just protocol
    _                 -> Nothing

containsAProtocolUpdate
  :: Implementation sip impl => impl -> Bool
containsAProtocolUpdate = isJust . proposedProtocolUpdate

data ImplementationType impl
  = Cancellation { toCancel :: ![ProtocolId impl] }
  -- ^ Note that a non-protocol update can't be canceled since they enter into
  -- effect as soon as they are approved.
  | Protocol !(Protocol impl)
  | NonProtocol !(NonProtocol impl)

deriving instance Implementation sip impl => Show (ImplementationType impl)

-- | On the constraints:
--
-- * the activation protocol relies on the ordering of the proposal's versions
--
-- * endorsers should be able to be put in ordered collections.
--
class ( Ord (Version protocol)
      , Show (Version protocol)
      , Show (Endorser protocol)
      , Eq protocol
      , Ord protocol
      , Show protocol

      , Identifiable protocol
      , Identifiable (Endorser protocol)
      ) => Activable protocol where

  data Endorser protocol

  data Version protocol :: Type
  version :: protocol -> Version protocol

  -- | Protocol identifier that the protocol.
  supersedesId :: protocol -> Id protocol
  -- | Protocol version that the protocol.
  supersedesVersion :: protocol -> Version protocol

type ProtocolId p = Id (Protocol p)

type EndorserId p = Id (Endorser p)

--------------------------------------------------------------------------------
-- Serialisation instances
--------------------------------------------------------------------------------

instance
  ( Typeable impl
  , ToCBOR (Protocol impl)
  , ToCBOR (Id (Protocol impl))
  , ToCBOR (NonProtocol impl)
  ) => ToCBOR (ImplementationType impl) where
  toCBOR (Cancellation ps) =
    encodeListLen 2 <> encodeWord 0 <> toCBOR ps
  toCBOR (Protocol p) =
    encodeListLen 2 <> encodeWord 1 <> toCBOR p
  toCBOR (NonProtocol a) =
    encodeListLen 2 <> encodeWord 2 <> toCBOR a

instance
  ( Typeable impl
  , FromCBOR (Protocol impl)
  , FromCBOR (Id (Protocol impl))
  , FromCBOR (NonProtocol impl)
  ) => FromCBOR (ImplementationType impl) where
  fromCBOR = do
    decodeListLenOf 2
    tag <- decodeWord
    case tag of
      0 -> Cancellation <$> fromCBOR
      1 -> Protocol     <$> fromCBOR
      2 -> NonProtocol  <$> fromCBOR
      _ -> fail $ "Unknown tag (" <> show tag <> ") when decoding a value of type 'ImplementationType'"
