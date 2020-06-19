{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Update.Proposal where

import           Cardano.Prelude (NoUnexpectedThunks)
import           Data.Maybe (isJust)
import           GHC.Generics (Generic)

import           Cardano.Slotting.Slot (SlotNo)

-- | Data for which a commit can be computed.
class ( Eq (Commit d)
      , Ord (Commit d)
      , Show (Commit d) ) => Commitable d where
  type Commit d :: *
  commit :: d -> Commit d

-- | Data for which an ID can be computed.
class ( Eq (Id p)
      , Ord (Id p)
      , Show (Id p) ) => Identifiable p where
  data Id p :: *
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

  data Submission proposal :: *
  data Revelation proposal :: *
  revelationCommit :: Submission proposal -> Commit (Revelation proposal)
  proposal :: Revelation proposal -> proposal

  votingPeriodDuration :: proposal -> SlotNo
  data Vote proposal :: *
  -- NOTE: if we define @Voter@ in this way, the stake distribution will have
  -- to go from @Voter@ to @Stake@. We will need something similar for
  -- endorsements. In this case we'd have an @Endorser@ type.
  data Voter proposal :: *
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
  deriving (Eq, Ord, Show, Enum, Generic, NoUnexpectedThunks)

--------------------------------------------------------------------------------
-- Implementation proposals
--------------------------------------------------------------------------------

-- | An implementation is associated to an predecessor proposal. The type of the
-- implementation uniquely identifies the type of its predecessor.
class ( Proposal impl
      , Activable (Protocol impl)
      , Show (Application impl)
      , Identifiable (Application impl)

      , Identifiable sip
      ) => Implementation sip impl | impl -> sip where

  -- | An implementation must refer to a sip proposal, which has a
  -- different type.
  preProposalId :: impl -> Id sip

  implementationType :: impl -> ImplementationType impl
  -- | Type of protocols the implementation implements.
  data Protocol impl :: *
  -- | Type of applications the implementation implements.
  data Application impl :: *

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
  -- ^ Note that you can't cancel an application update since they enter into
  -- effect as soon as they are approved.
  | Protocol !(Protocol impl)
  | Application !(Application impl)

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
      -- TODO: check that this is a reasonable constraint on the protocol types.
      -- It comes quite handy for using protocols as keys of ordered
      -- collections.
      , Eq protocol
      , Ord protocol
      , Show protocol

      , Identifiable protocol
      , Identifiable (Endorser protocol)
      ) => Activable protocol where

  -- This would replace @Hash p (VKey p)@. We could unify @Voter@ and @Endorser@
  -- into a common @Participant@ type, but it seems better to keep them separate.
  -- For instance, in a concrete instantiation the voting keys might be different
  -- from the endorsing keys.
  data Endorser protocol

  data Version protocol :: *
  version :: protocol -> Version protocol

  -- | Protocol identifier that the protocol.
  supersedesId :: protocol -> Id protocol
  -- | Protocol version that the protocol.
  supersedesVersion :: protocol -> Version protocol

type ProtocolId p = Id (Protocol p)

type EndorserId p = Id (Endorser p)
