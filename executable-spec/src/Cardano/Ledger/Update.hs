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
  , Activation.getCurrentVersion
  , Activation.isQueued
  , Activation.isBeingEndorsed
  , Activation.isScheduled
  , Activation.isTheCurrentVersion
  , Activation.candidateEndOfSafetyLag
  , Activation.isDiscardedDueToBeing
  , Activation.Reason (Expired, Canceled, Obsoleted, Unsupported)
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
import           GHC.Generics (Generic)

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

deriving instance (Proposal sip, Implementation sip impl) => Show (State sip impl)

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
