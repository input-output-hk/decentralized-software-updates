{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This shows the usage of variables for receiving and sending data.
module Datil.Examples.Echo where

import           Test.Tasty (TestTree, testGroup)

import           Control.State.DataAutomata
import           Control.State.DataAutomata.Expr
import           Control.State.DataAutomata.Interpreter.Memory
import           Control.State.DataAutomata.Interpreter.Run
import           Control.State.DataAutomata.Interpreter.Run.Test
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
