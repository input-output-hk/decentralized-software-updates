{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Trace
  ( -- * Function on traces
    Trace
  , scenario
  , initialState
  , transitionsList
  , validEvent
  , bootstrap
  , extend
  , reconstruct
  , validTransitions
  , validTransitionsWithPreviousStates
    -- * Functions on trace events
  , TraceEvent (Invalid, Valid, ensuingState)
  , extractState
    -- * Functions on trace fragments
  , TraceFragment
  , IsTraceFragment
  , validStates
  , firstState
  , lastState
  , validActions
  , invalidActions
  , firstAction
  , firstEvent
  , dropFirstValidAction
  )
where

import           Data.List (foldl')
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Sequence (Seq ((:|>), Empty), (|>))
import           GHC.Exts (toList)
import           GHC.Generics (Generic)

import           Cardano.Ledger.Assert (assertAndReturn, (==!))

import           SystemUnderTest (SUT, SUTAct, SUTSt, apply)
import           Trace.Scenario (Scenario)


-- | Trace for SUT @s@ generated using test scenario @t@.
data Trace s t =
  Trace
  { scenario     :: !(Scenario t)
  , initialState :: !(SUTSt s)
  , transitions  :: !(Seq (TraceEvent s))
    -- ^ Trace events. They appear in an oldest-first order. This is: the events
    -- appear in the order they occurred.
  } deriving (Generic)

deriving instance
  (Eq (SUTSt s), Eq (TraceEvent s), Eq (Scenario t))       => Eq (Trace s t)
deriving instance
  (Show (SUTSt s), Show (TraceEvent s), Show (Scenario t)) => Show (Trace s t)

--------------------------------------------------------------------------------
-- Trace event functions
--------------------------------------------------------------------------------

-- NOTE: we do not define trace events in a separate module because they do not
-- have a meaning without being associated to a trace. For instance, an invalid
-- action is relative to a previous state in a trace. Similarly, an ensuing
-- state assumes a predecessor state in the trace.

-- | Trace events.
data TraceEvent s
  = Invalid !(SUTAct s)
    -- ^ An invalid action in a trace signifies the fact that he action is
    -- invalid when applied to the state that immediately precedes this action.
  | Valid
    { action       :: !(SUTAct s)
    , ensuingState :: !(SUTSt s)
    }

deriving instance (Eq (SUTAct s), Eq (SUTSt s))     => Eq (TraceEvent s)
deriving instance (Show (SUTAct s), Show (SUTSt s)) => Show (TraceEvent s)

validEvent :: TraceEvent s -> Bool
validEvent (Invalid {}) = False
validEvent (Valid {})   = True

extractAction :: TraceEvent s -> SUTAct s
extractAction (Invalid act)      = act
extractAction (Valid { action }) = action

extractState :: TraceEvent s -> SUTSt s
extractState (Invalid {})               = error "Cannot extract an invalid state"
extractState (Valid   { ensuingState }) = ensuingState

--------------------------------------------------------------------------------
-- Trace functions
--------------------------------------------------------------------------------

-- | Initialize the trace with an initial state and sets the scenario that
-- describes the trace.
bootstrap :: Scenario t -> SUTSt s -> Trace s t
bootstrap scn st =
  Trace
  { scenario     = scn
  , initialState = st
  , transitions  = Empty
  }

transitionsList :: Trace s t -> [TraceEvent s]
transitionsList = toList . transitions

validTransitions :: Trace s t -> [(SUTAct s, SUTSt s)]
validTransitions = catMaybes . fmap asPair . transitionsList
  where
    asPair Invalid {}  = Nothing
    asPair (Valid a s) = Just (a, s)

validTransitionsWithPreviousStates :: Trace s t -> [(SUTSt s, SUTAct s, SUTSt s)]
validTransitionsWithPreviousStates trace
  = assertAndReturn postconditionsHold
  $ fmap (\(x, (y, z)) -> (x, y, z))
  $ zip (validStates trace) (validTransitions trace)
  where
    postconditionsHold xs = do
      -- We should also assert that the 3rd components of the result should
      -- coincide with the states of @validTransitions@, however this would
      -- require an @Eq@ constraint.
      length xs                  ==! length (validTransitions trace)
      length (validStates trace) ==! length (validTransitions trace) + 1

latestState :: Trace s t -> SUTSt s
latestState Trace { initialState, transitions } =
  fromMaybe initialState (latestStateOf transitions)
  where
    latestStateOf Empty                   = Nothing
    latestStateOf (ts :|> (Invalid _ ))   = latestStateOf ts
    latestStateOf (_  :|> (Valid _ st' )) = Just st'

extend :: SUT s => Trace s t -> SUTAct s -> Trace s t
extend trace act =
  trace { transitions =
          transitions trace |> case apply act (latestState trace) of
                                 Nothing     -> Invalid act
                                 Just nextSt -> Valid   act nextSt
        }

-- | Try to reconstruct a trace using the initial state and provided actions.
reconstruct
  :: SUT s
  => Scenario t
  -- ^ Scenario that originated the trace.
  -> SUTSt s
  -- ^ Initial state.
  -> [SUTAct s]
  -- ^ Actions to apply to the initial state.
  -> Trace s t
reconstruct setup st0 acts = foldl' extend (bootstrap setup st0) acts

--------------------------------------------------------------------------------
-- Functions on trace fragments
--------------------------------------------------------------------------------

type TraceFragment s = [TraceEvent s]

class IsTraceFragment f s | f -> s where
  validStates :: f -> [SUTSt s]

  firstState :: f -> SUTSt s

  lastState :: f -> SUTSt s

  -- | Return __all__ the actions in the trace, both __valid__ and __invalid__.
  -- The oldest action is returned first.
  allActions :: f -> [SUTAct s]

  -- | Return all the __valid__ actions in the fragment. The oldest action is
  -- returned first.
  validActions :: f -> [SUTAct s]

  invalidActions :: f -> [SUTAct s]

  firstAction :: f -> SUTAct s

  firstEvent :: f -> TraceEvent s

instance IsTraceFragment (Trace s t) s where
  validStates Trace { initialState, transitions } =
    initialState : (validStates $ toList transitions)

  firstState Trace { initialState } = initialState

  lastState = last . validStates

  allActions = allActions . transitionsList

  validActions = validActions . transitionsList

  invalidActions = invalidActions . transitionsList

  firstAction = firstAction . transitionsList

  firstEvent = firstEvent . transitionsList

instance IsTraceFragment (TraceFragment s) s where
  validStates = fmap extractState . filter validEvent

  firstState fragment =
    case validStates fragment of
      []   -> error "An empty fragment has no first state"
      st:_ -> st

  lastState fragment =
    case validStates fragment of
      xs@(_:_) -> last xs
      _        -> error $  "There are no states in the fragment."

  allActions = fmap extractAction

  validActions = fmap extractAction . filter validEvent

  invalidActions = fmap extractAction . filter (not . validEvent)

  firstAction fragment =
    case allActions fragment of
      []    -> error "firstAction: there is no first action in the fragment"
      act:_ -> act

  firstEvent []    = error "firstEvent: the fragment is empty"
  firstEvent (e:_) = e

dropFirstValidAction :: TraceFragment s -> TraceFragment s
dropFirstValidAction (x@Invalid {}:xs) = x:dropFirstValidAction xs
dropFirstValidAction (Valid {}:xs)     = xs
dropFirstValidAction []                = error "No valid action to drop"
