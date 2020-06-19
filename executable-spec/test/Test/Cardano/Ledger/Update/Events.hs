{-# LANGUAGE NamedFieldPuns #-}

-- | Functions on events associated to an update specification on a trace.
module Test.Cardano.Ledger.Update.Events
  ( UpdateEvent (E, eventState, eventFragment)
  , updateEvents
  , slotIs
  , validEventSlotIs
  , inAValidEventState
  )
where

import           Cardano.Slotting.Slot (SlotNo)

import           Cardano.Ledger.Update.Env.TracksSlotTime

import           Trace

import           Test.Cardano.Ledger.Update.Interface
import           Test.Cardano.Ledger.Update.Properties.SimpleScenario
import           Test.Cardano.Ledger.Update.Properties.UpdateSUT
import           Test.Cardano.Ledger.UpdateSpec

-- | Update events of an update specification.
--
-- An update event occurs when an the state of update specification, as reported
-- by the update system, changes. The state the update specification changed to
-- is stored in 'eventState'.
--
-- An update event has also a trace fragment ('TraceFragment') associated to it,
-- which contains the actions and state changes __at__ and after the event.
--
-- An update event can be obtained from an update specification and a trace.
data UpdateEvent
   = E { eventState    :: UpdateState
       -- ^ State that defines the event. An event occurs at the earliest state
       -- in which the state of an update proposal changes.
       , eventFragment :: TraceFragment UpdateSUT
       -- ^ Longest trace fragment for which the update specification is in
       -- 'eventState'.
       --
       -- Throughout the event fragment, the state of the update specification
       -- that correspond to the event should be equal to 'eventState'.
       }
   deriving (Show)

-- | Get the update events for the given update specification.
--
-- This function should always return an element.
updateEvents :: UpdateSpec -> Trace UpdateSUT Simple -> [UpdateEvent]
updateEvents updateSpec = go . transitionsList
  where
    go [] = []
    go xs
      | all (not . validEvent) xs = []
        -- If there are no valid events in the trace this means we cannot check
        -- the state of the proposal since there are no valid states in the
        -- trace.
      | otherwise                 =
          firstUpdateEvent: go fragmentNotInFirstEventState
      where
        firstUpdateEvent =
          E { eventState    = firstEventState
            , eventFragment = fragmentInFirstEventState
            }

        firstEventState = stateOf updateSpec
                        $ unUpdateSt
                        $ firstState xs

        (fragmentInFirstEventState, fragmentNotInFirstEventState)
          = span (inAnEventState ( (== firstEventState)
                                  . stateOf updateSpec
                                 )
                 )
                 xs

-- | Determine whether the slot of the event satisfies the predicate. If the
-- event is invalid, and therefore carries no state and has no associated slot,
-- then the function return true.
slotIs :: (SlotNo -> Bool) -> TraceEvent UpdateSUT -> Bool
slotIs p = inAnEventState (p . currentSlot)

-- | Determine whether the event is valid and the state of the event satisfies
-- the given predicate.
validEventSlotIs :: (SlotNo -> Bool) -> TraceEvent UpdateSUT -> Bool
validEventSlotIs p = inAValidEventState (p . currentSlot)

-- | Determine whether the state of the event satisfies the predicate. If the
-- event has no state (this is, it is an invalid event) then this function
-- returns true.
inAnEventState
  :: (IState -> Bool)
  -> TraceEvent UpdateSUT
  -> Bool
inAnEventState _ (Invalid {}            ) = True
inAnEventState p (Valid   {ensuingState}) = p . unUpdateSt $ ensuingState

-- | Determine whether the event is valid and its associated state satisfies the
-- predicate.
inAValidEventState
  :: (IState -> Bool)
  -> TraceEvent UpdateSUT
  -> Bool
inAValidEventState p evt = validEvent evt && inAnEventState p evt
