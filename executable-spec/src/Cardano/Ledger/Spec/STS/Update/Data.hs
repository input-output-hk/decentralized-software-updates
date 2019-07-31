{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardano.Ledger.Spec.STS.Update.Data where

--import Data.Word (Word64)
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           Data.Set (Set)
import           GHC.Generics (Generic)
import           Data.Text (Text)
import           Numeric.Natural (Natural)
import           Data.Hashable (Hashable)
import qualified Data.Hashable as H
import qualified Ledger.Core as Core
import           Data.Map.Strict (Map)


-- |A unique ID of a software update
newtype UpId = UpId { getUpId :: Integer --Word64
                    }
  deriving (Show, Eq, Ord, Hashable, Generic)

instance Core.HasHash (UpId) where
  hash (UpId i) = Core.Hash $ H.hash i

instance Num UpId where
  (+) a b = UpId $ (getUpId a) + (getUpId b)

-- | Protocol version
data ProtVer = ProtVer
  { _pvMaj :: Natural
  , _pvMin :: Natural
  , _pvAlt :: Natural
  } deriving (Eq, Generic, Ord, Show, Hashable)

-- | Application version
newtype ApVer = ApVer Natural
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Num, Hashable)

-- | Consensus Protocol Parameter Name
data ParamName =
    BlockSizeMax
  | TxSizeMax
  | SlotSize
  | EpochSize
  deriving (Eq, Generic, Ord, Show, Hashable)

-- | Metadata structure for SIP
data SIPMetadata =
  SIPMetadata { versionFrom :: (ProtVer, ApVer)
                -- ^ The version the this SIP has been based on
              , versionTo :: (ProtVer, ApVer)
              -- ^ the version after the SIP takes effect
              , impactsConsensus :: Bool
              -- ^ Flag to determine an impact on the underlying consensus protocol
              , impactsParameters :: [ParamName]
              -- ^ List of protocol parameters impacted
              } deriving (Eq, Generic, Ord, Show, Hashable)

-- | Contents of a SIP
data SIPData =
  SIPData {  url :: Text
            -- ^ URL pointing at the SIP
          , metadata :: SIPMetadata
            -- ^ SIP Metadata
          }
  | NullSIPData
  deriving (Eq, Generic, Ord, Show, Hashable)

instance Core.HasHash (SIPData) where
  hash a = Core.Hash $ H.hash a

-- | System improvement proposal
data SIP =
  SIP { id :: UpId
      -- ^ Submission proposal id.
      , sipHash :: Core.Hash
      -- ^ Hash of the SIP contents (`SIPData`)
      , author :: Core.VKey
      -- ^ Who submitted the proposal.
      , salt :: Int
      -- ^ The salt used during the commit phase
      }
  deriving (Eq, Generic, Ord, Show, Hashable)

instance Core.HasHash (SIP) where
  hash a = Core.Hash $ H.hash a

-- | A commitment data type.
-- It is the `hash` $ salt ++ sip_owner_pk ++ `hash` `SIP`
newtype Commit = Commit { getCommit :: Core.Hash}
  deriving (Show, Eq, Ord)

-- | The System improvement proposal at the commit phase
data SIPCommit =
  SIPCommit { _commit :: Commit
            -- ^ A salted commitment (a hash) to the SIP id, the public key
            -- and the `hash` `SIP` (H(salt||pk||H(SIP)))
            , _author :: Core.VKey
            -- ^ Who submitted the proposal.
            , _upSig :: Core.Sig Commit
            -- ^ A signature on commit by the author public key
            }
  deriving (Eq, Show, Ord)

-- | Calculate a `Commit` from a `SIP`
calcCommit :: SIP -> Commit
calcCommit sip =
  Commit $
    Core.hash $
      -- Limit calculation of commit only to the id and author field, otherwise you dont get a match at reveal signal!
      --(myshow . salt $ sip)
      (myshow . Cardano.Ledger.Spec.STS.Update.Data.id $ sip)
      `mappend` (myshow . author $ sip)
      --`mappend` (myshow . Core.hash $ sip)

-- | Ideation phase state
data State
  = State
    { commitedSIPs :: !(Map Commit SIPCommit)
    , submittedSIPs :: !(Set SIP)
    , revealedSIPs :: !(Set SIP)
    }
  deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup State
  deriving Monoid via GenericMonoid State

-- | Ideation signals.
data Signal
  = Submit SIPCommit SIP
  | Reveal SIP
  deriving (Eq, Ord, Show)

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
