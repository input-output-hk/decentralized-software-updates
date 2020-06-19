{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Trace.Scenario
  ( HasScenario
  , Scenario
  , ElaboratorSt
  , mkInitStates
  , canTerminate
  , nextAct
  )
where

import           SystemUnderTest

-- | Systems under test for which a trace can be uniquely determined for any
-- given scenario.
--
-- We use different type parameters to allow systems under test to define
-- multiple such scenarios.
class SUT s => HasScenario s t where

  -- | Setup for the SUT tests. A test setup might include things like:
  --
  -- * Parameter values: e.g. stability parameter, slots per-epoch,
  --   participants, initial stake distribution, etc.
  --
  -- * Model setup: e.g. how many updates are we going to run in parallel? What
  --   is the dependency between these updates.
  --
  -- Using functions @mkInitialState@, @canTerminate@, and @nextAct@ it is
  -- possible to construct a 'Trace' for the SUT. The test setup uniquely
  -- determines a trace. See module @Properties@ for more details.
  --
  data Scenario t

  -- | Elaborator state of the model.
  --
  -- An elaborator requires a state to determine the next action that will be
  -- elaborated. See 'nextAct'.
  data ElaboratorSt t

  -- | Get initial elaborator and SUT states from the test setup.
  --
  -- The initial elaborator state can be used as the starting point for
  -- elaborating a trace.
  --
  -- The initial SUT state can be used as the initial state of a trace.
  mkInitStates :: Scenario t -> (ElaboratorSt t, SUTSt s)

  -- | Determine whether we can terminate the trace elaboration.
  canTerminate :: ElaboratorSt t -> Bool

  nextAct :: Scenario t -> ElaboratorSt t -> (SUTAct s, ElaboratorSt t)
