{-# LANGUAGE DeriveGeneric #-}

module Test.Cardano.Ledger.Update.UnitTests.Common where

import           Control.Arrow (left)
import           Control.Monad.Except (liftEither)
import           Control.Monad.State (get, gets, put)
import           GHC.Generics (Generic)

import           Cardano.Slotting.Slot (SlotNo)

import           Test.Cardano.Ledger.Update.Interface hiding (tickTill)
import           Test.Cardano.Ledger.Update.TestCase


--------------------------------------------------------------------------------
-- Clock ticking functions
--------------------------------------------------------------------------------

tickTillStable :: TestCase
tickTillStable =
  gets currentSlotStableAt >>= tickTill

tickFor :: SlotNo -> TestCase
tickFor numberOfSlots = do
  currentSlot <- gets iStateCurrentSlot
  tickTill (currentSlot + numberOfSlots)

tickTill :: SlotNo -> TestCase
tickTill endAt = do
  currentSlot <- gets iStateCurrentSlot
  if endAt <= currentSlot
    then pure ()
    else do
      tick (currentSlot + 1)
      tickTill endAt

tickTillNextEpoch :: TestCase
tickTillNextEpoch = do
  epochFirstSlot <- gets iStateEpochFirstSlot
  slotsPerEpoch  <- gets iStateSlotsPerEpoch
  let nextEpochBegins = epochFirstSlot + slotsPerEpoch
  tickTill nextEpochBegins

tick :: SlotNo -> TestCase
tick slot = do
  st  <- get
  st' <- liftEither $ left (`UpdateError` st) $ slotTick slot st
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
