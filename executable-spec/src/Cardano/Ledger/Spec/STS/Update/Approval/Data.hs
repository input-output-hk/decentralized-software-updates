{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Update.Approval.Data where

import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           Cardano.Binary (ToCBOR (toCBOR), encodeListLen)

import           Data.AbstractSize (HasTypeReps)

import           Cardano.Ledger.Spec.Classes.HasAuthor (HasAuthor, author)
import           Cardano.Ledger.Spec.Classes.Hashable (Hash, Hashable)
import           Cardano.Ledger.Spec.Classes.HasSalt (HasSalt, salt)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     Signature, VKey)
import           Cardano.Ledger.Spec.State.Ballot (Ballot)
import           Cardano.Ledger.Spec.State.ProposalsState (ProposalsState)
import           Cardano.Ledger.Spec.State.ProposalState (HasVotingPeriod,
                     IsVote, getConfidence, getVoter, getVotingPeriodDuration)
import           Cardano.Ledger.Spec.STS.Update.Data (Confidence, URL)
import           Cardano.Ledger.Spec.STS.Update.Data.Commit (Commit)
import           Cardano.Ledger.Spec.STS.Update.Ideation.Data (SIPData, SIPHash)
import           Ledger.Core (SlotCount)


type ImplBallot p = Ballot p (Hash p (ImplementationData p))

type IPSSt p = ProposalsState p (ImplementationData p)

--------------------------------------------------------------------------------
-- STS data
--------------------------------------------------------------------------------

data Payload p
  = Submit
    { commit  :: !(Commit p (Implementation p))
    , sAuthor :: !(VKey p)
    , sig     :: !(Signature p (Commit p (Implementation p)))
    }
  | Reveal (Implementation p)
  | Vote (ImplVote p)
  deriving (Generic, Typeable)

type SignedVoteData p = ( Hash p (ImplementationData p)
                        , Confidence
                        , VKey p
                        )

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
  { implDataSIPHash :: SIPHash p
    -- ^ Reference to the SIP that is being implemented.
  , implDataVPD     :: !SlotCount
    -- ^ Vote period duration for the implementation proposal.
  , implURL         :: URL
    -- ^ URL that points to the location where the implementation code can be
    -- found.
 , implCodeHash    :: CodeHash
    -- ^ Hash of the implementation code. The hash of the code 'implURL' points
    -- to must be the same as this one.
  } deriving (Eq, Show, Generic)

implSIPHash :: Implementation p -> SIPHash p
implSIPHash = implDataSIPHash . implPayload

type CodeHash = Int -- TODO: we need to define what this will be.

instance HasVotingPeriod (ImplementationData p) where
  getVotingPeriodDuration = implDataVPD

data ImplVote p =
  ImplVote
    { vImplHash  :: !(Hash p (ImplementationData p))
      -- ^ Implementation that is being voted for.
    , confidence :: !Confidence
    , vAuthor    :: !(VKey p)
    , vSig       :: !(Signature p (SignedVoteData p))
    }
  deriving (Generic, Typeable)

deriving instance (Hashable p, HasSigningScheme p) => Show (ImplVote p)

instance IsVote p (ImplVote p) where
  getVoter = vAuthor

  getConfidence = confidence

--------------------------------------------------------------------------------
-- HasTypeReps instances
--------------------------------------------------------------------------------

deriving instance
  ( Typeable p
  , HasTypeReps p
  , HasTypeReps (VKey p)
  , HasTypeReps (Signature p (Commit p (Implementation p)))
  , HasTypeReps (Implementation p)
  , HasTypeReps (ImplVote p)
  ) => HasTypeReps (Payload p)

instance ( Typeable p
         , HasTypeReps (VKey p)
         , HasTypeReps (ImplementationData p)
         ) => HasTypeReps (Implementation p)

instance ( Typeable p
         , HasTypeReps (Hash p SIPData)
         ) => HasTypeReps (ImplementationData p)

instance ( Typeable p
         , HasTypeReps (VKey p)
         , HasTypeReps (Hash p (ImplementationData p))
         , HasTypeReps (Signature p (SignedVoteData p))
         ) => HasTypeReps (ImplVote p)

--------------------------------------------------------------------------------
-- ToCBOR instances
--------------------------------------------------------------------------------

instance ( Typeable p
         , ToCBOR (VKey p)
         , ToCBOR (Hash p SIPData)
         ) => ToCBOR (Implementation p) where
  toCBOR Implementation { implAuthor, implSalt, implPayload }
    =  encodeListLen 3
    <> toCBOR implAuthor
    <> toCBOR implSalt
    <> toCBOR implPayload


instance ( Typeable p
         , ToCBOR (Hash p SIPData)
         ) => ToCBOR (ImplementationData p) where
  toCBOR ImplementationData { implDataSIPHash, implDataVPD }
    =  encodeListLen 2
    <> toCBOR implDataSIPHash
    <> toCBOR implDataVPD
