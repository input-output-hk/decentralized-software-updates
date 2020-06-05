{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Update.Interface

where

import           Data.Bifunctor (bimap)
import           GHC.Generics (Generic)

import           Ledger.Core (BlockCount, (*.), (+.))
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.HasAdversarialStakeRatio
                     (HasAdversarialStakeRatio, adversarialStakeRatio)
import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.Classes.HasStakeDistribution
                     (HasStakeDistribution, SIPExperts (SIPExperts),
                     StakePools (StakePools),
                     TechnicalExperts (TechnicalExperts), stakeDistribution)
import           Cardano.Ledger.Spec.Classes.HasVotingPeriodsCap
                     (HasVotingPeriodsCap, maxVotingPeriods)
import           Cardano.Ledger.Spec.Classes.TracksSlotTime (TracksSlotTime,
                     currentSlot, epochFirstSlot, nextEpochFirstSlot,
                     stableAfter)
import qualified Cardano.Ledger.Spec.Classes.TracksSlotTime as SlotTime
import           Cardano.Ledger.Spec.State.ProposalState (VotingPeriod)
import           Cardano.Ledger.Spec.State.ProposalState (Decision)
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import qualified Cardano.Ledger.Spec.State.StakeDistribution as STS.StakeDistribution
import           Cardano.Ledger.Spec.STS.Update.Approval.Data
                     (ImplementationAndHash, ProtocolVersion)
import qualified Cardano.Ledger.Spec.STS.Update.Approval.Data as Approval.Data

import qualified Cardano.Ledger.Update as Update
-- TODO: we shouldn't need to import the modules below:
import qualified Cardano.Ledger.Update.Approval as Approval
import qualified Cardano.Ledger.Update.Ideation as Ideation

import           Cardano.Ledger.Assert

-- | Interface state:
--
-- - it contains values needed by the environment and state of the SOS rules.
-- - it is modified either by the application of the SOS rules.
--
data IState p =
  IState
  { iStateK :: !BlockCount
  , iStateMaxVotingPeriods :: !VotingPeriod
  , iStateStakeDist :: !(STS.StakeDistribution.StakeDistribution p)
  , iStateCurrentSlot :: !Core.Slot
  , iStateEpochFirstSlot :: !Core.Slot
  , iStateSlotsPerEpoch :: !Core.SlotCount
  , iStateR_a :: !Float
  , iStateStakepoolsDistribution :: !(StakeDistribution p)

  , updateSt :: !(Update.State p)
  } deriving (Show)

iStateProtocolVersion :: Hashable p => IState p -> ProtocolVersion
iStateProtocolVersion = Approval.Data.version . iStateCurrentVersion

iStateCurrentVersion :: IState p -> ImplementationAndHash p
iStateCurrentVersion = Update.getCurrentVersion . updateSt

-- | At which slot is will the current slot (according to the given state)
-- become stable.
currentSlotStableAt :: IState p -> Core.Slot
currentSlotStableAt st = stableAt (iStateCurrentSlot st) st

-- | At which slot will the given slot become stable.
--
stableAt :: Core.Slot -> IState p -> Core.Slot
stableAt slot st = slot +. stableAfter st

-- | Is the given slot stable?
isStable :: Core.Slot -> IState p -> Bool
isStable slot st = stableAt slot st <= iStateCurrentSlot st

instance TracksSlotTime (IState p) where
  currentSlot = iStateCurrentSlot

  stableAfter = (2 *.) . iStateK

  slotsPerEpoch = iStateSlotsPerEpoch

  epochFirstSlot = iStateEpochFirstSlot

instance HasStakeDistribution SIPExperts (IState p) p where
  stakeDistribution SIPExperts = iStateStakeDist

-- | TODO: we need to introduce a stake distribution for experts in the
-- environment. For now we're using the SIP experts stake distribution.
instance HasStakeDistribution TechnicalExperts (IState p) p where
  stakeDistribution TechnicalExperts = iStateStakeDist

instance HasStakeDistribution StakePools (IState p) p where
  stakeDistribution StakePools = iStateStakepoolsDistribution

instance HasAdversarialStakeRatio (IState p) where
  adversarialStakeRatio = iStateR_a

instance HasVotingPeriodsCap (IState p) where
  maxVotingPeriods = iStateMaxVotingPeriods

instance Update.HasIdeationState (IState p) p where
  getIdeationState = Update.getIdeationState . updateSt

instance Update.HasActivationState (IState p) p where
  getActivationState = Update.getActivationState . updateSt

instance Update.HasApprovalState (IState p) p where
  getApprovalState = Update.getApprovalState . updateSt

slotTick
  :: Hashable p => Core.Slot -> IState p -> Either (UIError p) (IState p)
slotTick slot iState
  -- TODO: with the addition of a @iStateEpochFirstSlot@ and
  -- @iStateSlotsPerEpoch@ we have to ensure we don't go back in time.
  --
  -- So the Hupdate rule (or probably a rule above it) should make sure we don't
  -- go back in time, and the invariants are satisfied.
  = assert preconditionsHold
  $ tickTill slot iState
  where
    preconditionsHold = do
      currentSlot iState <=! slot


tickTill
  :: Hashable p => Core.Slot -> IState p -> Either (UIError p) (IState p)
tickTill desiredSlot st
  | iStateCurrentSlot st == desiredSlot =
      pure st
  | iStateCurrentSlot st <  desiredSlot =
      tickTill desiredSlot
      $ modifyUpdateSt st'
      $ Update.tick st' (updateSt st')
  | otherwise                               =
    error "target slot should be less or equal than the current slot"
  where
    nextSlot = iStateCurrentSlot st +. 1
    st'  = SlotTime.checkInvariants
             $ st { iStateCurrentSlot    = nextSlot
                  , iStateEpochFirstSlot =
                      if nextEpochFirstSlot st <= nextSlot
                      then nextEpochFirstSlot st
                      else epochFirstSlot st
                  }



-- TODO: the payload parameter might be named 'payload' by the client, so we
-- might as well omit 'Payload' from the function name.
applyUpdatePayload
  :: ( Ideation.CanApply (IState p) p -- TODO: it'd be nice if we could only use
                                      -- Update constraints, without looking two
                                      -- levels down the hierarchy.
     , Approval.CanApply (IState p) p
     )
   => Update.Payload p -> IState p -> Either (UIError p) (IState p)
applyUpdatePayload payload st =
  bimap ApplyError (modifyUpdateSt st) $ Update.apply st payload (updateSt st)

modifyUpdateSt :: IState p -> Update.State p -> IState p
modifyUpdateSt st updateSt' = st { updateSt = updateSt' }

data UIError p
  = ApplyError { getUpdateError :: Update.Error p }
  deriving (Show)

instance Update.HasApprovalError (UIError p) p where
  getApprovalError = Update.getApprovalError . getUpdateError

instance Update.HasActivationError (UIError p) p where
  getActivationError = Update.getActivationError . getUpdateError

--------------------------------------------------------------------------------
-- Update state
--------------------------------------------------------------------------------

data UpdateState
  = Unknown
  -- ^ The update is not know to the update-state
  | SIP PhaseState
  | Implementation PhaseState
  | Queued
  | ActivationExpired
  | ActivationCanceled
  | ActivationUnsupported
  | BeingEndorsed
  | HasEnoughEndorsements
  | Scheduled
  | Activated
  | Canceled
  deriving (Eq, Show, Generic)



data PhaseState
  = Submitted
  | StablySubmitted
  | Revealed
  | StablyRevealed
  | Is Decision
  | IsStably Decision
  deriving (Eq, Show, Generic)
