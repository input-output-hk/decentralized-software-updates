module Cardano.Ledger.Update.Env.TracksSlotTime where

import           GHC.Stack (HasCallStack)
import           Control.Exception (assert)

import Cardano.Slotting.Slot (SlotNo)

-- | Classes of environments that keep track of slot time, and can determine the
-- stability of a slot.
class TracksSlotTime e where
  -- | Determine whether the given slot is stable in the environment.
  isStable :: e -> SlotNo ->  Bool
  isStable e givenSlot = stableAt e givenSlot  <= currentSlot e

  -- | Return the slot at which the given slot will become stable.
  stableAt :: e -> SlotNo -> SlotNo
  stableAt e givenSlot = givenSlot + stableAfter e

  -- | Number of slots after which an event can be considered stable. For
  -- instance, in Byron this was set to:
  --
  -- > 2 * k
  stableAfter :: e -> SlotNo

  currentSlot :: e -> SlotNo

  slotsPerEpoch :: e -> SlotNo

  epochFirstSlot :: e -> SlotNo

  nextEpochFirstSlot :: e -> SlotNo
  nextEpochFirstSlot e  =
    epochFirstSlot e + slotsPerEpoch e

  checkInvariants :: HasCallStack => e -> e
  checkInvariants e
    = assert (epochFirstSlot e <= currentSlot e)
    $ assert (currentSlot e < nextEpochFirstSlot e)
    $ e
