-- | Utilities for testing runnable models.

module Control.State.DataAutomata.Interpreter.Run.Test where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.State.DataAutomata
import           Control.State.DataAutomata.Interpreter.Run
import           Control.State.DataAutomata.Interpreter.Trace


-- | This can be used in combination with functions such as 'shouldEndInState'
-- or 'shouldDeadlockWhenRunning' to avoid repeating the model parameter when
-- one has a list of tests to run. For instance:
--
--
-- > with someModel $ [ xs `shouldEndInState` someSt, ys `shouldEndInState` someOtherSt ]
--
with :: RunnableModel -> [RunnableModel -> TestTree] -> [TestTree]
with model = fmap ( $ model )

-- | Run all the actions on the given model, and check whether the model results
-- in the expected state.
shouldEndInState
  :: [CAction]
  -- ^ Actions to run. First action to run is the first element of the list.
  -> LTree State
  -- ^ Expected state after running the given actions.
  -> RunnableModel
  -> TestTree
shouldEndInState acts expectedState model
  = testProperty (actionNames acts <> " ends in state " <> show expectedState)
  $ finalState (runModel model acts) == expectedState


actionNames :: [CAction] -> String
actionNames = show . fmap (unActionName . actionName)

-- | Run all the actions, and check whether we get a deadlock error after
-- trying to run the given actions.
--
-- TODO: we might want to add the pre-condition that the 'init' of the list __should__ not deadlock.
shouldDeadlockWhenRunning
  :: [CAction]
  -- ^ Actions to run. First action to run is the first element of the list.
  -> RunnableModel
  -> TestTree
shouldDeadlockWhenRunning acts model
  = testProperty (actionNames acts <> " deadlocks")
  $ isDeadlock
  $ getError
  $ runModel model acts
