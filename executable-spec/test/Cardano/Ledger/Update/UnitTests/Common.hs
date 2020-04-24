{-# LANGUAGE DeriveGeneric #-}

module Cardano.Ledger.Update.UnitTests.Common where

import           Control.Arrow (left)
import           Control.Monad.Except (liftEither)
import           Control.Monad.State (get, gets, put)
import           GHC.Generics (Generic)

import           Ledger.Core (Slot, SlotCount, (+.))

import           Cardano.Ledger.Update.Interface hiding (tickTill)

import           Cardano.Ledger.Update.TestCase


--------------------------------------------------------------------------------
-- Clock ticking functions
--------------------------------------------------------------------------------

tickTillStable :: TestCase
tickTillStable =
  gets currentSlotStableAt >>= tickTill

tickFor :: SlotCount -> TestCase
tickFor numberOfSlots = do
  currentSlot <- gets iStateCurrentSlot
  tickTill (currentSlot +. numberOfSlots)

tickTill :: Slot -> TestCase
tickTill endAt = do
  currentSlot <- gets iStateCurrentSlot
  if endAt <= currentSlot
    then pure ()
    else do
      tick (currentSlot +. 1)
      tickTill endAt

tickTillNextEpoch :: TestCase
tickTillNextEpoch = do
  epochFirstSlot <- gets iStateEpochFirstSlot
  slotsPerEpoch  <- gets iStateSlotsPerEpoch
  let nextEpochBegins = epochFirstSlot +. slotsPerEpoch
  tickTill nextEpochBegins

tick :: Slot -> TestCase
tick slot = do
  st  <- get
  st' <- liftEither $ left (`STSError` st) $ slotTick slot st
  put st'

--------------------------------------------------------------------------------
-- Update actions
--------------------------------------------------------------------------------

data UpdateAction
  = Submit
  | Reveal
  | Approve
  | Reject
  | Abstain
  deriving (Eq, Show, Generic)

submit, reveal, approve, reject, abstain :: UpdateAction
submit  = Submit
reveal  = Reveal
approve = Approve
reject  = Reject
abstain = Abstain
