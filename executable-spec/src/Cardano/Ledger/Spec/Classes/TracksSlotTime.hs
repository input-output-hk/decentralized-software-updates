module Cardano.Ledger.Spec.Classes.TracksSlotTime where

import GHC.Stack (HasCallStack)
import           Control.Exception (assert)

import           Ledger.Core (Slot, SlotCount, (+.))

-- | Classes of environments that keep track of slot time, and can determine the
-- stability of a slot.
class TracksSlotTime e where
  -- | Determine whether the given slot is stable in the environment.
  isStable :: e -> Slot ->  Bool
  isStable e givenSlot = stableAt e givenSlot  <= currentSlot e

  -- | Return the slot at which the given slot will become stable.
  stableAt :: e -> Slot -> Slot
  stableAt e givenSlot = givenSlot +. stableAfter e

  -- | Number of slots after which an event can be considered stable. For
  -- instance, in Byron this was set to:
  --
  -- > 2 * k
  stableAfter :: e -> SlotCount

  currentSlot :: e -> Slot

  slotsPerEpoch :: e -> SlotCount

  epochFirstSlot :: e -> Slot

  nextEpochFirstSlot :: e -> Slot
  nextEpochFirstSlot e  =
    epochFirstSlot e +. slotsPerEpoch e

  checkInvariants :: HasCallStack => e -> e
  checkInvariants e
    = assert (epochFirstSlot e <= currentSlot e)
    $ assert (currentSlot e < nextEpochFirstSlot e)
    $ e
