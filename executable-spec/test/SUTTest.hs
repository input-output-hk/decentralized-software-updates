{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module SUTTest where

import           Test.QuickCheck

-- | System under test
class SUT s where

  -- | State of the system under test
  data SUTSt s

  -- | Actions of the SUT
  data SUTAct s

  -- | Apply the action to the SUT, and return the ensuing state if the action
  -- could be successfully applied, 'Nothing' otherwise.
  apply :: SUTAct s -> SUTSt s -> Maybe (SUTSt s)

-- | Test generator for the SUT.
class SUT s => TestGen s t where

  -- | Setup for the SUT tests. A test setup includes:
  --
  -- * Parameter values: e.g. stability parameter, slots per-epoch, participants,
  --   initial stake distribution, etc.
  --
  -- * Model setup: e.g. how many updates are we going to run in parallel? What is
  --   the dependency between these updates.
  data TestSetup t

  -- | Generator state of the model.
  data GenSt t

  -- | Initialize the generator state from the test setup.
  --
  -- Should this return a @Gen@ monad, or an initial state? Since we're
  -- generating the test setup, it doesn't seem like a good idea to put another
  -- level of randomness. I'd say the test setup should uniquely identify the
  -- initial generator and SUT state.
  mkInitStates :: TestSetup t -> (GenSt t, SUTSt s)

  -- | Determine whether we can terminate the generation of action.
  canTerminate :: GenSt t -> Bool

  -- | Generate an action based on the current state. This generator should
  -- always be able to return an action based on the given generator state.
  -- Before calling 'genAct' the trace generation algorithm will call
  -- 'canTerminate'. If no further actions can be generated in a given state
  -- @st@, then @canTerminate st@ should hold.
  --
  genAct :: TestSetup t -> GenSt t -> Gen (SUTAct s)

  -- | Update the generator state with the given SUT state.
  --
  -- TODO: we rely on the property that @update sutSt@ is idempotent, this is:
  --
  -- > update (update genSt sutSt) sutSt == update genSt sutSt
  --
  update :: GenSt t -> SUTSt s -> GenSt t
