{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

-- TODO: this data is too concrete and should be removed. The test module might
-- define a simpler version of these modules (for instance without using hashes
-- or keys).
import qualified Cardano.Ledger.Spec.STS.Update.Approval.Data as Approval.Data
import qualified Cardano.Ledger.Spec.STS.Update.Ideation.Data as Ideation.Data
-- TODO: the Hashable class should be removed as well.
import           Cardano.Ledger.Spec.Classes.Hashable (Hash, Hashable)

-- Environment constraints
import           Cardano.Ledger.Spec.Classes.HasAdversarialStakeRatio
                     (HasAdversarialStakeRatio)
import           Cardano.Ledger.Spec.Classes.HasStakeDistribution
                     (HasStakeDistribution, SIPExperts, StakePools,
                     TechnicalExperts)
import           Cardano.Ledger.Spec.Classes.TracksSlotTime (TracksSlotTime)



import qualified Cardano.Ledger.Update.Activation as Activation
import qualified Cardano.Ledger.Update.Approval as Approval
import qualified Cardano.Ledger.Update.Ideation as Ideation

data State p =
  State
  { ideationSt   :: !(Ideation.State p)
  , approvalSt   :: !(Approval.State p)
  , activationSt :: !(Activation.State p)
  }
  deriving (Show, Generic)

data Error p
  = IdeationError (Ideation.Error p)
  | ApprovalError (Approval.Error p)
  | ActivationError (Activation.Error p)
  deriving (Generic, Show)

data Payload p
  = Ideation (Ideation.Data.Payload p)
  | Approval (Approval.Data.Payload p)
  | Activation (Activation.Endorsement p)

--------------------------------------------------------------------------------
-- State update functions
--------------------------------------------------------------------------------

initialState
  :: Hashable p
  => Hash p (Approval.Data.ImplementationData p)
  -> Approval.Data.ImplementationData p
  -- ^ Initial implementation. This determines the current version.
  -> State p
initialState initialHash initialImplData  =
  State
  { ideationSt = Ideation.initialState
  , approvalSt = Approval.initialState
  , activationSt = Activation.initialState initialHash initialImplData
  }

tick
  :: ( TracksSlotTime env
     , HasStakeDistribution SIPExperts env p
     , HasStakeDistribution TechnicalExperts env p
     , HasStakeDistribution StakePools env p
     , HasAdversarialStakeRatio env

     , Hashable p
     )
  => env -> State p -> State p
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
  :: ( Ideation.CanApply env p
     , Approval.CanApply env p
     )
  => env
  -> Payload p
  -> State p
  -> Either (Error p) (State p)
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

instance Ideation.HasIdeationState (State p) p where
  getIdeationState = ideationSt

instance Approval.HasApprovalState (State p) p where
  getApprovalState = approvalSt

instance Activation.HasActivationState (State p) p where
  getActivationState = activationSt

instance Approval.HasApprovalError (Error p) p where
  getApprovalError (ApprovalError err) = Just err
  getApprovalError _                   = Nothing

instance Activation.HasActivationError (Error p) p where
  getActivationError (ActivationError err) = Just err
  getActivationError _                     = Nothing
