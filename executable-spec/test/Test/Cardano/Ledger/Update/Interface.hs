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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Update.Interface

where

import           Data.Bifunctor (bimap)
import           GHC.Generics (Generic)

import           Cardano.Slotting.Block (BlockNo, unBlockNo)
import           Cardano.Slotting.Slot (SlotNo)

import           Cardano.Ledger.Update.Env.HasAdversarialStakeRatio
                     (HasAdversarialStakeRatio, adversarialStakeRatio)
import           Cardano.Ledger.Update.Env.HasStakeDistribution
                     (HasStakeDistribution, stakeDistribution)
import           Cardano.Ledger.Update.Env.HasVotingPeriodsCap
                     (HasVotingPeriodsCap, maxVotingPeriods)
import           Cardano.Ledger.Update.Env.StakeDistribution (StakeDistribution)
import           Cardano.Ledger.Update.Env.TracksSlotTime (TracksSlotTime,
                     currentSlot, epochFirstSlot, nextEpochFirstSlot,
                     stableAfter, stableAt)
import qualified Cardano.Ledger.Update.Env.TracksSlotTime as SlotTime
import           Cardano.Ledger.Update.ProposalsState (Decision, VotingPeriod)

import qualified Cardano.Ledger.Update as Update

import           Cardano.Ledger.Update.Proposal

import           Cardano.Ledger.Assert

-- So the first thing we need to do is to define data for test case.
--
-- The main use of this module is to provide the
--
-- > slotTick
--
-- and
--
-- > applyUpdate
--
-- functions.
--
-- So see what is an instance of Proposal we can define. I think we shouldn't
-- parametrize over the proposal. We can use a concrete type.
-- start here!!!

-- | Interface state:
--
-- - it contains values needed by the environment and state of the SOS rules.
-- - it is modified either by the application of the SOS rules.
--
-- @impl@ is the implementation.
data IState sip impl =
  IState
  { iStateK                :: !BlockNo
  , iStateMaxVotingPeriods :: !VotingPeriod
  , iStateSIPStakeDist     :: !(StakeDistribution (VoterId sip))
  , iStateImplStakeDist    :: !(StakeDistribution (VoterId impl))
  , iStateCurrentSlot      :: !SlotNo
  , iStateEpochFirstSlot   :: !SlotNo
  , iStateSlotsPerEpoch    :: !SlotNo
  , iStateR_a              :: !Float
  , iStateStakepoolsDistribution
                           :: !(StakeDistribution (EndorserId (Protocol impl)))
  , updateSt :: !(Update.State sip impl)
  }

deriving instance (Proposal sip, Implementation sip impl) => Show (IState sip impl)

iStateProtocolVersion :: (Implementation sip impl) => IState sip impl -> Version (Protocol impl)
iStateProtocolVersion = version . iStateCurrentVersion

iStateCurrentVersion :: Implementation sip impl => IState sip impl -> Protocol impl
iStateCurrentVersion = Update.getCurrentProtocol . updateSt

-- | At which slot is will the current slot (according to the given state)
-- become stable.
currentSlotStableAt :: IState sip impl -> SlotNo
currentSlotStableAt st = stableAt st (iStateCurrentSlot st)

-- | Is the given slot stable?
isStable :: SlotNo -> IState sip impl -> Bool
isStable slot st = stableAt st slot <= iStateCurrentSlot st

instance TracksSlotTime (IState sip impl) where
  currentSlot = iStateCurrentSlot

  stableAfter = (2 *) . fromIntegral . unBlockNo . iStateK

  slotsPerEpoch = iStateSlotsPerEpoch

  epochFirstSlot = iStateEpochFirstSlot

{-

-- TODO: We can't define these instances here since we'll get an overlapping
-- instances error :( So a possible solution might be to make IState monomorphic
-- if the polymorphism is not needed.

instance HasStakeDistribution (IState sip impl) (VoterId sip) where
  stakeDistribution = iStateSIPStakeDist

instance HasStakeDistribution (IState sip impl) (VoterId impl) where
  stakeDistribution = iStateImplStakeDist

-}

instance HasStakeDistribution (IState sip impl) (Id (Endorser (Protocol impl))) where
  stakeDistribution = iStateStakepoolsDistribution

instance HasAdversarialStakeRatio (IState sip impl) where
  adversarialStakeRatio = iStateR_a

instance HasVotingPeriodsCap (IState sip impl) where
  maxVotingPeriods = iStateMaxVotingPeriods

instance Update.HasIdeationState (IState sip impl) sip where
  getIdeationState = Update.getIdeationState . updateSt

instance Update.HasApprovalState (IState sip impl) impl where
  getApprovalState = Update.getApprovalState . updateSt

instance Implementation sip impl
         => Update.HasActivationState (IState sip impl) sip impl where
  getActivationState = Update.getActivationState . updateSt

slotTick
  :: ( HasStakeDistribution (IState sip impl) (VoterId sip)
     , HasStakeDistribution (IState sip impl) (VoterId impl)
     , Proposal sip
     , Implementation sip impl
     )
  => SlotNo -> IState sip impl -> Either (UIError sip impl) (IState sip impl)
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
  :: ( HasStakeDistribution (IState sip impl) (VoterId sip)
     , HasStakeDistribution (IState sip impl) (VoterId impl)
     , Proposal sip
     , Implementation sip impl
     )
  => SlotNo -> IState sip impl -> Either (UIError sip impl) (IState sip impl)
tickTill desiredSlot st
  | iStateCurrentSlot st == desiredSlot =
      pure st
  | iStateCurrentSlot st <  desiredSlot =
      tickTill desiredSlot
      $ modifyUpdateSt st'
      $ Update.tick st' (updateSt st')
  | otherwise                           =
    error "target slot should be less or equal than the current slot"
  where
    nextSlot = iStateCurrentSlot st + 1
    st'      = SlotTime.checkInvariants
             $ st { iStateCurrentSlot    = nextSlot
                  , iStateEpochFirstSlot =
                      if nextEpochFirstSlot st <= nextSlot
                      then nextEpochFirstSlot st
                      else epochFirstSlot st
                  }

applyUpdate
  :: ( Proposal sip
     , Implementation sip impl
     )
  => Update.Payload sip impl
  -> IState sip impl
  -> Either (UIError sip impl) (IState sip impl)
applyUpdate payload st =
  bimap ApplyError (modifyUpdateSt st) $ Update.apply st payload (updateSt st)

modifyUpdateSt
  :: IState sip impl -> Update.State sip impl -> IState sip impl
modifyUpdateSt st updateSt' = st { updateSt = updateSt' }

data UIError sip impl
  = ApplyError { getUpdateError :: Update.Error sip impl }

deriving instance (Proposal sip, Implementation sip impl) => Show (UIError sip impl)

instance Update.HasApprovalError (UIError sip impl) sip impl where
  getApprovalError = Update.getApprovalError . getUpdateError

instance Update.HasActivationError (UIError sip impl) sip impl where
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
  | Obsoleted
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
