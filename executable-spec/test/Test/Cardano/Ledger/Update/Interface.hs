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

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR),
                     decodeListLenOf, encodeListLen)
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
import           Cardano.Ledger.Update.Proposal
import           Cardano.Ledger.Update.ProposalsState (Decision, VotingPeriod)

import qualified Cardano.Ledger.Update as Update

import           Cardano.Ledger.Assert

import           Test.Cardano.Ledger.Update.Data


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
data IState =
  IState
  { iStateK                :: !BlockNo
  , iStateMaxVotingPeriods :: !VotingPeriod
  , iStateSIPStakeDist     :: !(StakeDistribution (VoterId MockSIP))
  , iStateImplStakeDist    :: !(StakeDistribution (VoterId MockImpl))
  , iStateCurrentSlot      :: !SlotNo
  , iStateEpochFirstSlot   :: !SlotNo
  , iStateSlotsPerEpoch    :: !SlotNo
  , iStateR_a              :: !Float
  , iStateStakepoolsDistribution
                           :: !(StakeDistribution (EndorserId (Protocol MockImpl)))
  , updateSt :: !(Update.State MockSIP MockImpl)
  }

deriving instance (Proposal MockSIP, Implementation MockSIP MockImpl) => Show IState
deriving instance (Proposal MockSIP, Implementation MockSIP MockImpl) => Eq IState

iStateProtocolVersion :: IState -> Version (Protocol MockImpl)
iStateProtocolVersion = version . iStateCurrentVersion

iStateCurrentVersion :: IState -> Protocol MockImpl
iStateCurrentVersion = Update.getCurrentProtocol . updateSt

-- | At which slot is will the current slot (according to the given state)
-- become stable.
currentSlotStableAt :: IState -> SlotNo
currentSlotStableAt st = stableAt st (iStateCurrentSlot st)

-- | Is the given slot stable?
isStable :: SlotNo -> IState -> Bool
isStable slot st = stableAt st slot <= iStateCurrentSlot st

instance TracksSlotTime IState where
  currentSlot = iStateCurrentSlot

  stableAfter = (2 *) . fromIntegral . unBlockNo . iStateK

  slotsPerEpoch = iStateSlotsPerEpoch

  epochFirstSlot = iStateEpochFirstSlot

instance HasStakeDistribution IState (VoterId MockSIP) where
  stakeDistribution = iStateSIPStakeDist

instance HasStakeDistribution IState (VoterId MockImpl) where
  stakeDistribution = iStateImplStakeDist

instance HasStakeDistribution IState (Id (Endorser (Protocol MockImpl))) where
  stakeDistribution = iStateStakepoolsDistribution

instance HasAdversarialStakeRatio IState where
  adversarialStakeRatio = iStateR_a

instance HasVotingPeriodsCap IState where
  maxVotingPeriods = iStateMaxVotingPeriods

instance Update.HasIdeationState IState MockSIP where
  getIdeationState = Update.getIdeationState . updateSt

instance Update.HasApprovalState IState MockImpl where
  getApprovalState = Update.getApprovalState . updateSt

instance Implementation MockSIP MockImpl
         => Update.HasActivationState IState MockSIP MockImpl where
  getActivationState = Update.getActivationState . updateSt

slotTick :: SlotNo -> IState -> Either UIError IState
slotTick slot iState
  = assert preconditionsHold
  $ tickTill slot iState
  where
    preconditionsHold = do
      currentSlot iState <=! slot

tickTill :: SlotNo -> IState -> Either UIError IState
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
  :: Update.Payload MockSIP MockImpl -> IState -> Either UIError IState
applyUpdate payload st =
  bimap ApplyError (modifyUpdateSt st) $ Update.apply st payload (updateSt st)

modifyUpdateSt
  :: IState -> Update.State MockSIP MockImpl -> IState
modifyUpdateSt st updateSt' = st { updateSt = updateSt' }

data UIError
  = ApplyError { getUpdateError :: Update.Error MockSIP MockImpl }
  deriving (Show)

instance Update.HasApprovalError UIError MockSIP MockImpl where
  getApprovalError = Update.getApprovalError . getUpdateError

instance Update.HasActivationError UIError MockSIP MockImpl where
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
  deriving (Eq, Ord, Show, Generic)

data PhaseState
  = Submitted
  | StablySubmitted
  | Revealed
  | StablyRevealed
  | Is Decision
  | IsStably Decision
  deriving (Eq, Ord, Show, Generic)

--------------------------------------------------------------------------------
-- Serialisation instances
--------------------------------------------------------------------------------

instance ToCBOR IState where
  toCBOR st =  encodeListLen 10
            <> toCBOR (iStateK st)
            <> toCBOR (iStateMaxVotingPeriods st)
            <> toCBOR (iStateSIPStakeDist st)
            <> toCBOR (iStateImplStakeDist st)
            <> toCBOR (iStateCurrentSlot st)
            <> toCBOR (iStateEpochFirstSlot st)
            <> toCBOR (iStateSlotsPerEpoch st)
            <> toCBOR (iStateR_a st)
            <> toCBOR (iStateStakepoolsDistribution st)
            <> toCBOR (updateSt st)

instance FromCBOR IState where
  fromCBOR = do
    decodeListLenOf 10
    k   <- fromCBOR
    mvp <- fromCBOR
    sds <- fromCBOR
    sdi <- fromCBOR
    cs  <- fromCBOR
    ef  <- fromCBOR
    sp  <- fromCBOR
    ra  <- fromCBOR
    spd <- fromCBOR
    ust <- fromCBOR
    return $! IState k mvp sds sdi cs ef sp ra spd ust
