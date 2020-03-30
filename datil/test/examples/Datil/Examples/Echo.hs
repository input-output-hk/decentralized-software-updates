{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This shows the usage of variables for receiving and sending data.
module Datil.Examples.Echo where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.State.DataAutomata
import           Control.State.DataAutomata.Expr
import           Control.State.DataAutomata.Interpreter.Memory
import           Control.State.DataAutomata.Interpreter.Run
import           Control.State.DataAutomata.Interpreter.Trace


echo :: DataAutomaton
echo =
  DataAutomaton
  { start       = "Waiting"
  , transitions =
    [ "Waiting" .-- ( "in"  #?  msg
                    , buf .= msg                     ) .--> "Echoing"
    , "Echoing" .-- ( "out" #!! VarE buf             ) .--> "Waiting"
    ]
  }
  where
    msg :: Var String
    msg = "msg"

    buf :: Var String
    buf = "buf"

runnableEcho :: RunnableModel
runnableEcho =
  RunnableModel
  { initialMemory = Leaf [("buf", Cell (undefined :: String))]
  , automata      = Single echo
  }


-- | Helper operator to make the tests look more readable and avoid boilerplate.
(?!) :: ActionName -> String -> CAction
action ?! val = CAction action val

--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

tests :: TestTree
tests
  = testGroup "echo"
  $ with runnableEcho
  $ [ []                   `shouldEndInState` "Waiting"

    , [ "in"  ?! "hello" ] `shouldEndInState` "Echoing"

    , [ "in"  ?! "hello"
      , "out" ?! "hello" ] `shouldEndInState` "Waiting"

    , [ "in"  ?! "foo"
      , "out" ?! "foo"
      , "in"  ?! "bar"
      , "out" ?! "bar" ]   `shouldEndInState` "Waiting"

    , shouldDeadlockWhenRunning
      [ "in"  ?! "hello"
      , "out" ?! "world" ]

    , shouldDeadlockWhenRunning
      [ "out"  ?! "Surprise!" ]

    , shouldDeadlockWhenRunning
      [ "in"  ?! "foo"
      , "out" ?! "foo"
      , "in"  ?! "marco"
      , "out" ?! "polo" ]
    ]

--------------------------------------------------------------------------------
-- Generic testing utilities. TODO: We should include this in the datil lib
--------------------------------------------------------------------------------

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
