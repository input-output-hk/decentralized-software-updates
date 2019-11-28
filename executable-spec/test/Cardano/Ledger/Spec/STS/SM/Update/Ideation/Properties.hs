{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Ledger.Spec.STS.SM.Update.Ideation.Properties where

import           Control.Monad.Except (Except)
import           Data.Maybe (fromJust)
import           Test.QuickCheck hiding (Success)

import           Control.State.DataAutomata
import           Control.State.DataAutomata.Interpreter.Gen hiding (Error)
import           Control.State.DataAutomata.Interpreter.Memory hiding (Error)
import           Control.State.DataAutomata.Interpreter.Run
import           Control.State.DataAutomata.Interpreter.Trace

import           Cardano.Ledger.Spec.SM.Ideation


--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

commitReveal' :: DataAutomaton
commitReveal' = commitReveal autId

autId :: Word
autId = 9999

crExampleRun0 :: Except Error (Memory, State)
crExampleRun0 = run (initCommitRevealMem 2 0)
                    (commitReveal autId)
                    [ (CAction  "tick" (2 :: Word))
                    , (CAction  "tick" (4 :: Word))
                    , (CAction  "tick" (10 :: Word))
                    , (CAction ("submit".@ autId) ())
                    , (CAction  "tick" (10 :: Word))
                    , (CAction  "tick" (12 :: Word))
                    , (CAction  "tick" (19 :: Word))
                    , (CAction ("reveal" .@ autId) ())
                    ]


example0 :: Property
example0 = finalState crExampleRun0 === "Revealed"

tickerCrSampleRun0 :: Except (LTree State, Error) (LTree Memory, LTree State)
tickerCrSampleRun0 =
  runAutomata (crFullModelInitMem 2 0)
              (crFullModel autId)
              [ (CAction "setInc" (2 :: Word))
              , (CAction "tick" (2 :: Word))
              , (CAction "tick" (4 :: Word))
              , (CAction ("submit" .@ autId) ())
              , (CAction "setInc" (10 :: Word))
              , (CAction "tick" (14 :: Word))
              , (CAction "tick" (24 :: Word))
              , (CAction ("reveal" .@ autId) ())
              ]

example1 :: Property
example1 = finalState tickerCrSampleRun0 === Leaf "Ticking" :++ Leaf "Revealed"

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

-- | Property that shows that a reveal action can be generated reached (and
-- therefore, the "Revealed" state can be reached).
prop_noReveals :: Property
prop_noReveals
  = expectFailure
  $ forAllTraces (Desired 50)
                 tickerActsGen
                 (crFullModelInitMem k 0)
                 (crFullModel autId)
                 submitNotReached
  where
    submitNotReached trace = ("reveal" .@ autId) `notElem` fmap actionName trace

-- | Value of k used in the tests, which must coincide with the @k@ value in
-- 'initCommitRevealMem'.
--
-- This could be improved by having a transition which sets the @k@ value for
-- every automata that needs it, which might be cumbersome, or having an
-- environment which is also generated. In the latter case, the environment
-- would be part of what's generated, together with the trace.
--
-- As a third alternative, @k@ might be injected in the memory by the generator,
-- and all we need will be to define a "reader" automata that reads the values
-- of the generated constants we're interested in (a disadvantage of this is
-- that the @"read k"@ actions will appear anywhere in the trace).
--
-- So maybe it's better to return the trace along with the initial environment.
-- This means that the shrink function will have to shrink pairs of initial
-- environments and traces, as opposed to shrinking traces only.
k :: Word
k = 2

prop_revealsStableSubmissions :: Property
prop_revealsStableSubmissions
  = withMaxSuccess 10000
  $ forAllTraces (Desired 50)
                 [ ("setInc", Cell <$> (elements [1 .. (3 :: Word)])) ]
                 (crFullModelInitMem 2 0)
                 (crFullModel autId)
                 submissionStable
  where
    submissionStable trace =
      Success ===
      case ( slotOfAction ("submit" .@ autId) trace, slotOfAction ("reveal" .@ autId) trace) of
        (Nothing, Nothing)     -> Success -- No submits, no reveals
        (Nothing, Just _)      -> RevealWihoutSubmit
        (Just _, Nothing)      -> Success
        (Just slotSubmit, Just slotReveal) ->
          if slotSubmit + 2 * k <= slotReveal then Success else Unstable

-- TODO: we need to check that we cover a sufficient number of:
--
-- - Submits with reveals.
-- - Submits without reveals.
-- - No submissions.
--

data StableSubmissionCheckResult = Success | RevealWihoutSubmit | Unstable
  deriving (Eq, Show)

-- | Find the slot at which the action occurs, defaulting to 0. Returns Nothing
-- if no such action was found.
--
slotOfAction :: ActionName -> Trace -> Maybe Word
slotOfAction name trace =
  if actionInTrace name trace
  then
    case actionsPriorTo name trace of
      [] -> Just 0
      xs ->
        case lastAction "tick" xs of
          Nothing    -> Just 0 -- There's no tick before the submission
          Just anAct -> Just $ fromJust (concreteActionValue anAct)
  else
    Nothing
