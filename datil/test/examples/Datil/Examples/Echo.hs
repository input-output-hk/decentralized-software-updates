{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This shows the usage of variables for receiving and sending data.
module Datil.Examples.Echo where

import           Test.QuickCheck (Gen, arbitrary)
import           Test.Tasty (TestTree, testGroup)

import           Control.State.DataAutomata
import           Control.State.DataAutomata.Expr
import           Control.State.DataAutomata.Interpreter.Gen
import           Control.State.DataAutomata.Interpreter.Memory
import           Control.State.DataAutomata.Interpreter.Run
import           Control.State.DataAutomata.Interpreter.Trace

import           Control.State.DataAutomata.Test.Properties
import           Control.State.DataAutomata.Test.Run


--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

runnableEcho :: RunnableModel
runnableEcho =
  RunnableModel
  { initialMemory = Leaf [("buf", Cell (undefined :: String))]
  , automata      = Single echo
  }

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

-- | Helper operator to make the tests look more readable and avoid boilerplate.
(?!) :: ActionName -> String -> CAction
action ?! val = CAction action val

--------------------------------------------------------------------------------
-- Property tests
--------------------------------------------------------------------------------

testableEcho :: GeneratorModel
testableEcho =
  GeneratorModel
  -- Note that unlike the @PingPong@ example, this model has a "in" input
  -- action. Unlike output actions, which can be calculated, input actions refer
  -- to external input, and as such, they have to be calculated.
  { actionGenerators = [ ("in", Cell <$> (arbitrary :: Gen String))]
  , runnableModel    = runnableEcho
  }


propertyTests :: TestTree
propertyTests
  = testGroup "echo"
  $ with testableEcho
  [ actionIsTriggered 100 (Desired 2) "in"
  , actionIsTriggered 100 (Desired 2) "out"
  ]
