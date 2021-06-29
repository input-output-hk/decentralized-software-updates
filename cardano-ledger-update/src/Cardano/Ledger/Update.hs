{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Update
  ( State
  , Error
  , Payload (Ideation, Approval, Activation)
    -- * State update functions
  , initialState
  , tick
  , apply
    -- * State query functions
    -- ** Ideation-state query functions
  , Ideation.HasIdeationState (getIdeationState)
  , Ideation.isSIPSubmitted
  , Ideation.isSIPStablySubmitted
  , Ideation.isSIPRevealed
  , Ideation.isSIPStablyRevealed
  , Ideation.isSIP
  , Ideation.isSIPStably
    -- ** Approval-state query functions
  , Approval.HasApprovalState (getApprovalState)
  , Approval.isImplementationSubmitted
  , Approval.isImplementationStablySubmitted
  , Approval.isImplementationRevealed
  , Approval.isImplementationStablyRevealed
  , Approval.isImplementation
  , Approval.isImplementationStably
    -- ** Activation-state query functions
  , Activation.Endorsement (Endorsement, endorserId, endorsedVersion)
  , Activation.HasActivationState (getActivationState)
  , Activation.getCurrentProtocol
  , Activation.getCurrentProtocolId
  , Activation.getCurrentProtocolVersion
  , Activation.isQueued
  , Activation.queuedProtocols
  , Activation.candidateProtocols
  , Activation.isBeingEndorsed
  , Activation.isScheduled
  , Activation.scheduledProtocol
  , Activation.scheduledProtocolVersion
  , Activation.isTheCurrentVersion
  , Activation.candidateEndOfSafetyLag
  , Activation.isDiscardedDueToBeing
  , Activation.endOfSafetyLag
  , Activation.Reason (Expired, Canceled, Unsupported)
  -- ** Approval-error query functions
  , Approval.HasApprovalError (getApprovalError)
  , Approval.noApprovedSIP
  , Approval.noImplementationCommit
  , Approval.noStableImplementationCommit
  , Approval.implementationVotePeriodHasNotStarted
  , Approval.implementationVotePeriodHasEnded
  -- ** Activation-error query functions
  , Activation.HasActivationError (getActivationError)
  , Activation.endorsedVersionError
  )
where

import           Control.Arrow (left)
import           Control.DeepSeq (NFData)
import           Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR),
                     decodeListLenOf, encodeListLen)

-- Environment constraints
import           Cardano.Ledger.Update.Env.HasAdversarialStakeRatio
                     (HasAdversarialStakeRatio)
import           Cardano.Ledger.Update.Env.HasStakeDistribution
                     (HasStakeDistribution)
import           Cardano.Ledger.Update.Env.HasVotingPeriodsCap
                     (HasVotingPeriodsCap)
import           Cardano.Ledger.Update.Env.TracksSlotTime (TracksSlotTime)

-- Update phases.
import qualified Cardano.Ledger.Update.Activation as Activation
import qualified Cardano.Ledger.Update.Approval as Approval
import qualified Cardano.Ledger.Update.Ideation as Ideation

import           Cardano.Ledger.Update.Proposal hiding (Payload)
import qualified Cardano.Ledger.Update.Proposal as Proposal

data State sip impl =
  State
  { ideationSt   :: !(Ideation.State sip)
  , approvalSt   :: !(Approval.State impl)
  , activationSt :: !(Activation.State sip impl)
  }
  deriving (Generic)

deriving instance
  ( Proposal sip
  , Implementation sip impl
  ) => Show (State sip impl)

deriving instance
  ( Proposal sip
  , Implementation sip impl
  , Eq sip
  , Eq impl
  , Eq (NonProtocol impl)
  ) => Eq (State sip impl)

deriving instance
  ( NFData sip
  , NFData impl
  , NFData (Id sip)
  , NFData (Id impl)
  , NFData (Id (Voter sip))
  , NFData (Id (Voter impl))
  , NFData (Id (Endorser (Protocol impl)))
  , NFData (Protocol impl)
  , NFData (NonProtocol impl)
  , NFData (Version (Protocol impl))
  , NFData (Commit (Revelation sip))
  , NFData (Commit (Revelation impl))
  ) => NFData (State sip impl)

deriving instance
  ( NoThunks sip
  , NoThunks impl
  , NoThunks (Id sip)
  , NoThunks (Id impl)
  , NoThunks (Id (Voter sip))
  , NoThunks (Id (Voter impl))
  , NoThunks (Id (Endorser (Protocol impl)))
  , NoThunks (Voter sip)
  , NoThunks (Voter impl)
  , NoThunks (Protocol impl)
  , NoThunks (NonProtocol impl)
  , NoThunks (Version (Protocol impl))
  , NoThunks (Commit (Revelation sip))
  , NoThunks (Commit (Revelation impl))
  ) => NoThunks (State sip impl)

deriving instance
  ( ToJSON sip
  , ToJSON impl
  , ToJSON (Id impl)
  , ToJSON (Id (Endorser (Protocol impl)))
  , ToJSON (Protocol impl)
  , ToJSONKey (Commit (Revelation sip))
  , ToJSONKey (Commit (Revelation impl))
  , ToJSONKey (Version (Protocol impl))
  , ToJSON (NonProtocol impl)
  , ToJSONKey (Protocol impl)
  , ToJSONKey (Id sip)
  , ToJSONKey (Id impl)
  , ToJSONKey (Id (Voter sip))
  , ToJSONKey (Id (Voter impl))
  ) => ToJSON (State sip impl)

deriving instance
  ( Proposal sip
  , Implementation sip impl
  , FromJSON sip
  , FromJSON impl
  , FromJSON (Id impl)
  , FromJSON (Id (Endorser (Protocol impl)))
  , FromJSON (Protocol impl)
  , FromJSONKey (Commit (Revelation sip))
  , FromJSONKey (Commit (Revelation impl))
  , FromJSONKey (Version (Protocol impl))
  , FromJSON (NonProtocol impl)
  , FromJSONKey (Protocol impl)
  , FromJSONKey (Id sip)
  , FromJSONKey (Id impl)
  , FromJSONKey (Id (Voter sip))
  , FromJSONKey (Id (Voter impl))
  ) => FromJSON (State sip impl)

data Error sip impl
  = IdeationError (Ideation.Error sip)
  | ApprovalError (Approval.Error sip impl)
  | ActivationError (Activation.Error sip impl)
  deriving (Generic)

deriving instance (Proposal sip, Implementation sip impl) => Show (Error sip impl)

data Payload sip impl
  = Ideation (Proposal.Payload sip)
  | Approval (Proposal.Payload impl)
  | Activation (Activation.Endorsement sip impl)

deriving instance (Proposal sip, Implementation sip impl)
                  => Show (Payload sip impl)

--------------------------------------------------------------------------------
-- State update functions
--------------------------------------------------------------------------------

initialState
  :: (Proposal sip, Implementation sip impl)
  => Protocol impl
  -- ^ Initial protocol. This determines the current version.
  -> State sip impl
initialState initialProtocol =
  State
  { ideationSt = Ideation.initialState
  , approvalSt = Approval.initialState
  , activationSt = Activation.initialState initialProtocol
  }

-- | Register the passage of time. The last applied slot of the activation state
-- is set to the slot provided in the environment.
--
-- The activation rules require that we do not miss a slot, so the slot given in
-- the environment should be equal to the last applied slot (which is stored in
-- the state) plus one.
tick
  :: ( TracksSlotTime env
     , HasStakeDistribution env (VoterId sip)
     , HasStakeDistribution env (VoterId impl)
     , HasStakeDistribution env (EndorserId (Protocol impl))
     , HasAdversarialStakeRatio env

     , Proposal sip
     , Implementation sip impl
     )
  => env -> State sip impl -> State sip impl
tick env st =
  let
    ideationSt'   = Ideation.tick env (ideationSt st)
    approvalSt'   = Approval.tick env (approvalSt st)
    activationSt' = Activation.tick env (activationSt st)

    ( approvalSt'', activationSt'')
      = Activation.transferApprovals env approvalSt' activationSt'
  in
    st { ideationSt   = ideationSt'
       , approvalSt   = approvalSt''
       , activationSt = activationSt''
       }

apply
  :: ( TracksSlotTime env
     , HasVotingPeriodsCap env
     , Proposal sip
     , Implementation sip impl
     )
  => env
  -> Payload sip impl
  -> State sip impl
  -> Either (Error sip impl) (State sip impl)
apply env (Ideation payload) st = do
  ideationSt' <- left IdeationError
                 $ Ideation.apply env payload (ideationSt st)
  pure $ st { ideationSt = ideationSt' }
apply env (Approval payload) st = do
  approvalSt' <- left ApprovalError
                 $ Approval.apply env (ideationSt st) payload (approvalSt st)
  pure $ st { approvalSt = approvalSt' }
apply env (Activation payload) st = do
  activationSt' <- left ActivationError
                   $ Activation.endorse env payload (activationSt st)
  pure $ st { activationSt = activationSt' }

--------------------------------------------------------------------------------
-- State query functions
--------------------------------------------------------------------------------

instance Ideation.HasIdeationState (State sip impl) sip where
  getIdeationState = ideationSt

instance Approval.HasApprovalState (State sip impl) impl where
  getApprovalState = approvalSt

instance Implementation sip impl
         => Activation.HasActivationState (State sip impl) sip impl where
  getActivationState = activationSt

instance Approval.HasApprovalError (Error sip impl) sip impl where
  getApprovalError (ApprovalError err) = Just err
  getApprovalError _                   = Nothing

instance Activation.HasActivationError (Error sip impl) sip impl where
  getActivationError (ActivationError err) = Just err
  getActivationError _                     = Nothing

--------------------------------------------------------------------------------
-- Serialisation instances
--------------------------------------------------------------------------------

instance
  ( Typeable sip
  , Typeable impl
  , Proposal sip
  , Proposal impl
  , Activable (Protocol impl)
  , ToCBOR (Protocol impl)
  , ToCBOR (Version (Protocol impl))
  , ToCBOR (NonProtocol impl)
  , ToCBOR sip
  , ToCBOR impl
  , ToCBOR (Id sip)
  , ToCBOR (Id impl)
  , ToCBOR (Id (Voter sip))
  , ToCBOR (Id (Voter impl))
  , ToCBOR (Id (Endorser (Protocol impl)))
  , ToCBOR (Commit (Revelation sip))
  , ToCBOR (Commit (Revelation impl))
  ) => ToCBOR (State sip impl) where
  toCBOR st =  encodeListLen 3
            <> toCBOR (ideationSt st)
            <> toCBOR (approvalSt st)
            <> toCBOR (activationSt st)

instance
  ( Typeable sip
  , Typeable impl
  , Proposal sip
  , Proposal impl
  , Activable (Protocol impl)
  , FromCBOR (Protocol impl)
  , FromCBOR (Version (Protocol impl))
  , FromCBOR (NonProtocol impl)
  , FromCBOR sip
  , FromCBOR impl
  , FromCBOR (Id sip)
  , FromCBOR (Id impl)
  , FromCBOR (Id (Voter sip))
  , FromCBOR (Id (Voter impl))
  , FromCBOR (Id (Endorser (Protocol impl)))
  , FromCBOR (Commit (Revelation sip))
  , FromCBOR (Commit (Revelation impl))
  ) => FromCBOR (State sip impl) where
  fromCBOR = do
    decodeListLenOf 3
    idst <- fromCBOR
    apst <- fromCBOR
    acst <- fromCBOR
    return $! State idst apst acst
