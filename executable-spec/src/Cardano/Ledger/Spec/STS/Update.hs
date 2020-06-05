{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Update where

import           GHC.Generics (Generic)
import           Data.Typeable (Typeable)

import           Control.State.Transition (Embed, Environment, PredicateFailure, failBecause,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)
import           Data.AbstractSize (HasTypeReps)
import           Ledger.Core (Slot, BlockCount, (*.), SlotCount)

import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme)
import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import qualified Cardano.Ledger.Spec.STS.Update.Approval as Approval
import qualified Cardano.Ledger.Spec.STS.Update.Approval.Data as Approval.Data
import qualified Cardano.Ledger.Spec.STS.Update.Ideation.Data as Ideation.Data
import           Cardano.Ledger.Spec.STS.Update.Approval (APPROVAL)
import           Cardano.Ledger.Spec.State.ProposalState (VotingPeriod)
import qualified Cardano.Ledger.Spec.State.ActivationState as Activation
import           Cardano.Ledger.Spec.State.ActivationState (ActivationState)
import           Cardano.Ledger.Spec.Classes.TracksSlotTime (TracksSlotTime)
import qualified Cardano.Ledger.Spec.Classes.TracksSlotTime as SlotTime
import           Cardano.Ledger.Spec.STS.CanExtract (CanExtract, extractAll)
import qualified Cardano.Ledger.Update.Ideation as Ideation
import           Cardano.Ledger.Spec.Classes.HasVotingPeriodsCap (HasVotingPeriodsCap , maxVotingPeriods)

data UPDATE p

-- | See @Ideation.Env@ for more details on the meaning of each field.
data Env p
  = Env
    { k :: !BlockCount
    , envMaxVotingPeriods :: !VotingPeriod
    , currentSlot :: !Slot
    , stakeDist :: !(StakeDistribution p)
    , slotsPerEpoch :: !SlotCount
    , epochFirstSlot :: !Slot
    }
  deriving (Show, Generic)

instance TracksSlotTime (Env p) where
  stableAfter    Env { k }              = 2 *. k
  currentSlot    Env { currentSlot }    = currentSlot
  slotsPerEpoch  Env { slotsPerEpoch }  = slotsPerEpoch
  epochFirstSlot Env { epochFirstSlot } = epochFirstSlot

instance HasVotingPeriodsCap (Env p) where
  maxVotingPeriods = envMaxVotingPeriods

data St p
  = St
    { ideationSt   :: !(Ideation.State p)
    , approvalSt   :: !(State (APPROVAL p))
    , activationSt :: !(ActivationState p)
    }
  deriving (Show, Generic)

data UpdatePayload p
  = Ideation (Ideation.Data.Payload p)
  | Approval (Approval.Data.Payload p)
  | Activation (Activation.Endorsement p)
  deriving (Show, Generic)

deriving instance ( Typeable p
                  , HasTypeReps (Ideation.Data.Payload p)
                  , HasTypeReps (Approval.Data.Payload p)
                  , HasTypeReps (Activation.Endorsement p)
                  ) => HasTypeReps (UpdatePayload p)

instance ( Typeable p
         , HasTypeReps (Ideation.Data.Payload p)
         , HasTypeReps (Approval.Data.Payload p)
         , HasTypeReps (Activation.Endorsement p)
         ) => Sized (UpdatePayload p) where
  costsList _
    =  costsList (undefined :: Ideation.Data.Payload p)

instance ( Hashable p
         , HasSigningScheme p
         , STS (APPROVAL p)

         -- TODO: we need to think how to bundle these constraints.
         , Ideation.CanApply (Env p) p
         ) => STS (UPDATE p) where

  type Environment (UPDATE p) = Env p

  type State (UPDATE p) = St p

  type Signal (UPDATE p) = UpdatePayload p

  data PredicateFailure (UPDATE p)
    = IdeationFailure (Ideation.Error p)
    | ApprovalFailure (PredicateFailure (APPROVAL p))
    | ActivationFailure (Activation.EndorsementError p)
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC ( env@Env { k
                    , envMaxVotingPeriods
                    , currentSlot
                    }
          , st@St { ideationSt
                  , approvalSt
                  , activationSt
                  }
          , update
          ) <- judgmentContext

      case update of
        Ideation ideationPayload -> do
          either (\ideationError -> do
                     failBecause (IdeationFailure ideationError)
                     pure st
                 )
                 (\ideationSt'   ->
                    pure $ st { ideationSt = ideationSt' }
                 )
                 (Ideation.apply env ideationPayload ideationSt)

        Approval approvalPayload ->
          do
            approvalSt' <-
              trans @(APPROVAL p) $ TRC ( Approval.Env k envMaxVotingPeriods currentSlot ideationSt
                                        , approvalSt
                                        , approvalPayload)
            pure st { approvalSt = approvalSt' }

        Activation endorsement ->
          either (\activationError -> do
                     failBecause (ActivationFailure activationError)
                     pure st
                 )
                 (\activationSt'   ->
                    pure $ st { activationSt = activationSt' }
                 )
                 (Activation.endorse endorsement env activationSt)
    ]

instance (STS (APPROVAL p), STS (UPDATE p)) => Embed (APPROVAL p) (UPDATE p) where
  wrapFailed = ApprovalFailure

data UPDATES p

instance ( Hashable p
         , HasSigningScheme p
         , STS (UPDATE p)
         ) => STS (UPDATES p) where

  type Environment (UPDATES p) = Environment (UPDATE p)

  type State (UPDATES p) = State (UPDATE p)

  type Signal (UPDATES p) = [Signal (UPDATE p)]

  data PredicateFailure (UPDATES p)
    = UpdateFailure (PredicateFailure (UPDATE p))
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC (env, st, updates) <- judgmentContext
      case updates of
        [] -> pure $! st
        (update:updates') ->
          do
            st' <- trans @(UPDATE p) $ TRC (env, st, update)
            trans @(UPDATES p) $ TRC (env, st', updates')
    ]


instance (STS (UPDATE p), STS (UPDATES p)) => Embed (UPDATE p) (UPDATES p) where
  wrapFailed = UpdateFailure

instance CanExtract (PredicateFailure (UPDATE p)) (PredicateFailure (APPROVAL p)) where
  extractAll (ApprovalFailure e) = [e]
  extractAll _                   = []

instance CanExtract (PredicateFailure (UPDATE p)) (Activation.EndorsementError p) where
  extractAll (ActivationFailure e) = [e]
  extractAll _                     = []
