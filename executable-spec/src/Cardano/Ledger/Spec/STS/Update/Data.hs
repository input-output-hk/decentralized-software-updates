{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Ledger.Spec.STS.Update.Data where

import qualified Data.Sequence as Seq
import Data.Word (Word64)
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           Data.Set (Set)
import           GHC.Generics (Generic)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Hashable (Hashable)
import qualified Data.Hashable as H
import           Data.Map.Strict (Map)
import           Data.Typeable   (typeOf)

import qualified Ledger.Core as Core
import           Data.AbstractSize (HasTypeReps, typeReps)

import Cardano.Ledger.Spec.STS.Sized (Sized, costsList)

data ImplementationPayload = ImplementationPayload
  deriving (Eq, Show, Generic, HasTypeReps)


instance Sized ImplementationPayload where
  -- TODO: define this properly
  costsList implementationPayload = [(typeOf implementationPayload, 10)]


-- | Ideation signals.
data IdeationPayload
  = Submit SIPCommit SIP
  | Reveal SIP
  | Vote SIP
  deriving (Eq, Ord, Show, Generic, HasTypeReps)

instance Sized IdeationPayload where
  -- TODO: define this properly
  costsList ideationPayload = [(typeOf ideationPayload, 10)]

-- | Protocol version
data ProtVer = ProtVer Word64
  deriving (Eq, Ord, Show)
  deriving stock (Generic)
  deriving anyclass (HasTypeReps, Hashable)
-- TODO: add these components.
{-  { _pvMaj :: Natural
  , _pvMin :: Natural
  , _pvAlt :: Word64
  }
-}


-- | Application version
newtype ApVer = ApVer Word64
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Num, Hashable)
  deriving anyclass (HasTypeReps)


-- | Consensus Protocol Parameter Name
data ParamName
  = BlockSizeMax
  | TxSizeMax
  | SlotSize
  | EpochSize
  deriving (Eq, Generic, Ord, Show, Hashable, HasTypeReps)


-- | Flag to distinguish between `SIP`s that impact or not the
-- underlying consensus protocol
data ConcensusImpact = Impact | NoImpact
  deriving (Eq, Generic, Ord, Show, Hashable, HasTypeReps)


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
              } deriving (Eq, Generic, Ord, Show, Hashable, HasTypeReps)


-- | Contents of a SIP
data SIPData =
  SIPData {  url :: !URL
            -- ^ URL pointing at the server where the SIP is stored
          , metadata :: !SIPMetadata
            -- ^ SIP Metadata (only core metadata, the rest are on the server
            -- pointed by the url)
          }

  deriving (Eq, Generic, Ord, Show, Hashable, HasTypeReps)

newtype URL = URL { getText :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

-- | This instance returns one 'Char' per-each character in the URL.
instance HasTypeReps URL where
  typeReps (URL text)
    = Seq.fromList
    $ typeOf (undefined :: URL)
      : replicate (T.length text) (typeOf (undefined :: Char))

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
  deriving (Eq, Generic, Ord, Show, Hashable, HasTypeReps)

instance Core.HasHash (SIP) where
  hash a = Core.Hash $ H.hash a


-- | A commitment data type.
-- It is the `hash` $ salt ++ sip_owner_pk ++ `hash` `SIP`
newtype Commit = Commit { getCommit :: Core.Hash}
  deriving stock (Generic)
  deriving (Show, Eq, Ord)
  deriving anyclass (HasTypeReps)


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
  deriving (Eq, Show, Ord, Generic, HasTypeReps)


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

-- | Ideation phase state
--
-- TODO: move this into ideation, and rename to St.
data State
  = State
    { commitedSIPs :: !(Map Commit SIPCommit)
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
  deriving Semigroup via GenericSemigroup State
  deriving Monoid via GenericMonoid State


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
