-- | Utilities for testing properties of the data automata

module Control.State.DataAutomata.Test.Properties where

import           Test.QuickCheck (Property, expectFailure, withMaxSuccess)
import           Test.Tasty (TestTree)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.State.DataAutomata
import           Control.State.DataAutomata.Interpreter.Gen
import           Control.State.DataAutomata.Interpreter.Run


-- | Check that an action is triggered.
--
-- TODO: at the moment we cannot check that the model reaches the given state
-- since intermediate states aren't included in the traces. We could modify
-- trace generation to correct this.
triggersAction :: TraceLength -> ActionName -> GeneratorModel -> Property
triggersAction traceLength anActName model
  = expectFailure
  $ forAllTraces traceLength
                 (actionGenerators                model)
                 (initialMemory . runnableModel $ model)
                 (automata      . runnableModel $ model)
                 (\trace -> anActName `notElem` fmap actionName trace)

actionIsTriggered
  :: Int
  -- ^ Maximum number of tests to run
  -> TraceLength
  -> ActionName
  -> GeneratorModel
  -> TestTree
actionIsTriggered maximumNumberOfTests traceLength anActName
  = testProperty (show (unActionName anActName) <> " is triggered")
  . withMaxSuccess maximumNumberOfTests
  . triggersAction traceLength anActName
