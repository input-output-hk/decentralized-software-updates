{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions to test conformance of a system under test w.r.t. a data automata
-- model.
module Control.State.DataAutomata.Test.Conformance where

import           Control.Monad (foldM)
import           Test.QuickCheck hiding (Success)

import           Control.State.DataAutomata.Interpreter.Trace

data ConformanceError sosErr iState
  = UnexpectedSOSError CAction sosErr
  | UnexpectedSOSSuccess CAction iState
  deriving (Show)

-- | Run the conformance test.
runTest
  :: forall iState sosErr
   . (iState -> CAction -> Either sosErr iState)
   -- TODO: we will need some additional information when calling the SUT, like
   -- the mapping from participant id's to actual keys.
   --
  -> iState
  -- ^ Interface state (SUT state).
  -> (InvalidTrace -> InvalidTrace)
  -- ^ Trace filter: not all actions translate to SUT actions. In particular,
  -- actions of the form @Left act@ where @act@ is an internal action cannot be
  -- translated to SUT calls since the SUT would need to return a failure on an
  -- invalid action.
  -> InvalidTrace
  -> Either (ConformanceError sosErr iState) iState
runTest callSUT st traceFilter trace = foldM step st (traceFilter trace)
  where
    step :: iState -> Either CAction CAction -> Either (ConformanceError sosErr iState) iState
    step st' (Left cAct) =
      case callSUT st' cAct of
        Left  _ -> Right st' -- The SUT also errors, this shows conformance.
        Right _ -> Left (UnexpectedSOSSuccess cAct st')
    step st' (Right cAct) =
      case callSUT st' cAct of
        Left err   -> Left (UnexpectedSOSError cAct err)
        Right st'' -> Right st''

-- | Conformance test.
conforms
  :: forall model params iState sosErr
   . (Show sosErr, Show iState)
  => (params -> iState)
  -- ^ Function used to bootstrap the initial state of the SUT
  -> (params -> model -> Gen InvalidTrace)
  -- ^ Trace generator
  -> (InvalidTrace -> InvalidTrace)
  -- ^ Trace filter. See 'runTest'.
  -> (iState -> CAction -> Either sosErr iState)
  -- ^ Function used to call the SUT
  -> model
  -- ^ SUT model
  -> params
  -- ^ Model parameters
  -> Property
conforms mIState traceGen traceFilter callSUT model params =
  forAll (traceGen params model)
         (checkResult . runTest callSUT initSt traceFilter)
  where
    initSt = mIState params

    checkResult (Left err) =
      counterexample (show err)
      $ False
    checkResult (Right _) = property ()
