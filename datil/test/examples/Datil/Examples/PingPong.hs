{-# LANGUAGE OverloadedStrings #-}

-- | Trivial automata without data
module Datil.Examples.PingPong where

import           Test.Tasty (TestTree, testGroup)

import           Control.State.DataAutomata

import           Control.State.DataAutomata.Interpreter.Run

import           Control.State.DataAutomata.Test.Run


pingPong :: DataAutomaton
pingPong =
  DataAutomaton
  { start       = "Ping"
  , transitions =
    [ "Ping" .-- "ping" #! () .--> "Pong"
    , "Pong" .-- "pong" #! () .--> "Ping"
    -- The way the data automata formalism deals with no determinism is by
    -- scanning for matching actions from top to bottom in the transitions list.
    -- The @Pung!@ state will never be reached.
    , "Pong" .-- "pong" #! () .--> "Pung!"
    ]
  }

runnablePingPong :: RunnableModel
runnablePingPong =
  RunnableModel
  { initialMemory = Leaf mempty -- No variables in this model.
  , automata      = Single pingPong
  }

--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

tests :: TestTree
tests
  = testGroup "ping-pong"
  $ with runnablePingPong
  [ []                       `shouldEndInState` "Ping"
  , ["ping"]                 `shouldEndInState` "Pong"
  , ["ping", "pong"]         `shouldEndInState` "Ping"
  , ["ping", "pong", "ping"] `shouldEndInState` "Pong"

  , shouldDeadlockWhenRunning ["pong"]
  , shouldDeadlockWhenRunning ["ping", "ping"]
  , shouldDeadlockWhenRunning ["ping", "pong", "pong"]
  ]
