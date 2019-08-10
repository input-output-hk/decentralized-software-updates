{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Ledger.Spec.STS.Update.Data where

import Data.Word (Word64)
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           Data.Set (Set)
import           GHC.Generics (Generic)
import           Data.Text (Text)
--import           Numeric.Natural (Natural)
import           Data.Hashable (Hashable)
import qualified Data.Hashable as H
import qualified Ledger.Core as Core
import           Data.Map.Strict (Map)
import           Ledger.Core (Slot (Slot))
import           Data.Bimap (Bimap, (!))
import           Cardano.Prelude (HeapWords, heapWords, heapWords2, heapWords3, heapWords4)

-- |A unique ID of a software update
-- newtype UpId = UpId { getUpId :: Word64}
--   deriving (Show, Eq, Ord, Hashable, Generic)

-- instance Core.HasHash (UpId) where
--   hash (UpId i) = Core.Hash $ H.hash i

-- instance Num UpId where
--   (+) a b = UpId $ (getUpId a) + (getUpId b)

-- | Protocol version
data ProtVer = ProtVer Word64
{-  { _pvMaj :: Natural
  , _pvMin :: Natural
  , _pvAlt :: Word64
  }
-}  deriving (Eq, Generic, Ord, Show, Hashable)

instance HeapWords ProtVer where
  heapWords _ = 1

-- | Application version
newtype ApVer = ApVer Word64
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Num, Hashable)

instance HeapWords ApVer where
  heapWords _ = 1

-- | Consensus Protocol Parameter Name
data ParamName =
    BlockSizeMax
  | TxSizeMax
  | SlotSize
  | EpochSize
  deriving (Eq, Generic, Ord, Show, Hashable)

instance HeapWords ParamName where
  heapWords _ = 1

-- Flag to distinguish between `SIP`s that impact or not the
-- underlying consensus protocol
data ConcensusImpact = Impact | NoImpact
  deriving (Eq, Generic, Ord, Show, Hashable)

instance HeapWords ConcensusImpact where
  heapWords _ = 1

-- | Metadata structure for SIP
data SIPMetadata =
  SIPMetadata { versionFrom :: !(ProtVer, ApVer)
                -- ^ The version the this SIP has been based on
              , versionTo :: !(ProtVer, ApVer)
              -- ^ the version after the SIP takes effect
              , impactsConsensus :: !ConcensusImpact
              -- ^ Flag to determine an impact on the underlying consensus protocol
              , impactsParameters :: !([ParamName])
              -- ^ List of protocol parameters impacted
              } deriving (Eq, Generic, Ord, Show, Hashable)

instance HeapWords SIPMetadata where
  heapWords (SIPMetadata f t c p) = heapWords4 f t c p

-- | Contents of a SIP
data SIPData =
  SIPData {  url :: !Text
            -- ^ URL pointing at the server where the SIP is stored
          , metadata :: !SIPMetadata
            -- ^ SIP Metadata (only core metadata,
            -- the rest are on the server pointed by the url)
          }
  deriving (Eq, Generic, Ord, Show, Hashable)

instance HeapWords SIPData where
  heapWords (SIPData u m) = heapWords2 u m

instance Core.HasHash (SIPData) where
  hash a = Core.Hash $ H.hash a

-- | System improvement proposal
data SIP =
  SIP { --id :: !UpId
        -- Submission proposal id.
        sipHash :: !Core.Hash
      -- ^ Hash of the SIP contents (`SIPData`)
      -- also plays the role of a SIP unique id
      , author :: !Core.VKey
      -- ^ Who submitted the proposal.
      , salt :: !Int
      -- ^ The salt used during the commit phase
      , sipPayload :: SIPData
      -- ^ The actual contents of the SIP.
      }
  deriving (Eq, Generic, Ord, Show, Hashable)

instance HeapWords SIP where
  heapWords (SIP (Core.Hash i) (Core.VKey (Core.Owner n)) s p) = heapWords4 (heapWords i) (heapWords n) s p

instance Core.HasHash (SIP) where
  hash a = Core.Hash $ H.hash a

-- | A commitment data type.
-- It is the `hash` $ salt ++ sip_owner_pk ++ `hash` `SIP`
newtype Commit = Commit { getCommit :: Core.Hash}
  deriving (Show, Eq, Ord)

instance HeapWords Commit where
  heapWords (Commit (Core.Hash i)) = heapWords i

-- | The System improvement proposal at the commit phase
data SIPCommit =
  SIPCommit { commit :: !Commit
            -- ^ A salted commitment (a hash) to the SIP id, the public key
            -- and the `hash` `SIP` (H(salt||pk||H(SIP)))
            , _author :: !Core.VKey
            -- ^ Who submitted the proposal.
            , upSig :: !(Core.Sig Commit)
            -- ^ A signature on commit by the author public key
            }
  deriving (Eq, Show, Ord)

instance HeapWords SIPCommit where
  heapWords (SIPCommit c (Core.VKey (Core.Owner n)) (Core.Sig cm (Core.Owner nn)))  = heapWords3 c (heapWords n) (heapWords2 cm nn)

-- | Calculate a `Commit` from a `SIP`
calcCommit :: SIP -> Commit
calcCommit sip =
  Commit $
    Core.hash $
      -- Calculation of commit based on the following fields.
      (myshow . salt $ sip)
      --`mappend` (myshow . Cardano.Ledger.Spec.STS.Update.Data.id $ sip)
      `mappend` (myshow . author $ sip)
      --`mappend` (myshow . sipHash $ sip)
      `mappend` (myshow . Core.hash $ sip)

-- | Ideation phase environment
data EnvIdeation
  = EnvIdeation
    { stakeholders :: Bimap Core.VKey Core.SKey
      -- ^ The set of participants, identified by their signing
      -- and verifying keys. There is a one-to-one correspondence
      -- between the signing and verifying keys, hence
      -- the use of 'Bimap'
    , currSlot :: Slot
      -- ^ The current slot in the blockchain
    }
  deriving (Eq, Show)

-- | Ideation phase state
data StIdeation
  = StIdeation
    { txToSlot :: Map UpdateTx Slot
      -- ^ Every Update transaction is included into
      -- a `Block`, which has been generated at a specific `Slot`.
    , commitedSIPs :: !(Map Commit SIPCommit)
      -- ^ These are the encrypted SIPs that are submitted at the commit phase
      -- of an SIP submission
    , submittedSIPs :: !(Set SIP)
      -- ^ These are the SIPs that we need to generate for the testing to
      -- take place. From these both the commitedSIP's as well as the revealedSIPs
      -- will be created. This state is not part of the update protocol, it is used
      -- only for SIP generation purposes.
    , revealedSIPs :: !(Set SIP)
    }
  deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup StIdeation --State
  deriving Monoid via GenericMonoid StIdeation -- State

-- | The union of all types that are update events.
-- All update events must be an instance of this.
class UpdateEvent a where

-- | A software update transaction
data UpdateTx
  = Ideation IdeationTx
  -- | Implementation ImplementationTx
  -- | Approval  ApprovalTx
  -- | Activation  ActivationTx
  deriving (Eq, Ord, Show)

instance HeapWords UpdateTx where
  heapWords (Ideation ideationtx) = heapWords ideationtx
  -- heapWords (Implementation impltx) = heapWords impltx
  -- heapWords (Approval apptx) = heapWords apptx
  -- heapWords (Activation acttx) = heapWords acttx


instance UpdateEvent UpdateTx where

-- | An Ideation phase transaction
data IdeationTx
  = Submit SIPCommit SIP
  | Reveal SIP
  deriving (Eq, Ord, Show)

instance HeapWords IdeationTx where
  heapWords (Submit sipcommit sip) = heapWords2 sipcommit sip
  heapWords (Reveal sip) = heapWords sip

instance UpdateEvent IdeationTx where

-- | TODO: An Implementation phase transaction
data ImplementationTx

instance UpdateEvent ImplementationTx where

-- | TODO: An Approval phase transaction
data ApprovalTx

instance UpdateEvent ApprovalTx where

-- | TODO: An Activation phase transaction
data ActivationTx

instance UpdateEvent ActivationTx where

-- | Ideation signals.
-- data Signal
--   = Submit SIPCommit SIP
--   | Reveal SIP
--   deriving (Eq, Ord, Show)

-- | A newtype string that is an instance of `HasHash`
newtype MyString = MyString { str :: String }
  deriving (Eq, Generic, Ord, Show)

instance Core.HasHash (MyString) where
  hash (MyString s) = Core.Hash $ H.hash s

instance Semigroup (MyString) where
  (<>) (MyString s1) (MyString s2) = MyString (s1 ++ s2)

instance Monoid (MyString) where
  mempty = MyString ""

-- | A `show` of `MyString`
myshow :: Show a => a -> MyString
myshow a = MyString $ show a
