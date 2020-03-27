{-# LANGUAGE OverloadedStrings #-}

-- | Trivial automata without data
module Datil.Examples.PingPong where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.State.DataAutomata
import           Control.State.DataAutomata.Interpreter.Run
import           Control.State.DataAutomata.Interpreter.Trace

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
  $ fmap check          [ ([]                      , "Ping")
                        , (["ping"]                , "Pong")
                        , (["ping", "pong"]        , "Ping")
                        , (["ping", "pong", "ping"], "Pong")
                        ]
  ++ fmap checkDeadlock [ (["pong"])
                        , (["ping", "ping"])
                        , (["ping", "pong", "pong"])
                        ]
  where
    check (xs, st) =
      testProperty ("Passes with " <> actionNames xs)
      (finalState (runPingPong xs) == Leaf st)

    actionNames = show . fmap unActionName

    checkDeadlock xs =
      testProperty ("Deadlocks with " <> actionNames xs)
      (isDeadlock $ getError (runPingPong xs))

    runPingPong xs = runModel runnablePingPong $ fmap (`CAction` ()) xs
