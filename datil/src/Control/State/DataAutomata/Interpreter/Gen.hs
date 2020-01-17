{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Control.State.DataAutomata.Interpreter.Gen where

import           Control.Applicative (Alternative, empty, (<|>))
import           Control.Arrow (first, left, second, (***))
import           Control.Monad (mfilter, when)
import           Control.Monad.Except (runExcept)
import           Data.Either (isRight, partitionEithers)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Text.Lazy as T
import           Data.Typeable (typeOf)
import           Test.QuickCheck
import           Text.Pretty.Simple (pPrint, pShow)

import           Control.State.DataAutomata
import           Control.State.DataAutomata.Expr
import           Control.State.DataAutomata.Interpreter.Memory hiding (Error)
import           Control.State.DataAutomata.Interpreter.Run hiding (Error)
import qualified Control.State.DataAutomata.Interpreter.Run as Run
import           Control.State.DataAutomata.Interpreter.Trace


actGen
  :: Map ActionName (Gen Cell)
  -> Memory
  -> Action
  -> Gen (Maybe CAction)
actGen trToGen _mem (Input name var) = do
  case Map.lookup name trToGen of
    Nothing -> do
      pure Nothing
    Just gen -> do
      Cell val <- gen
      let
        actualType = typeOf val
        expectedType = varType var
      when (actualType /= expectedType)
           (error $ "Generated type: " ++ show actualType ++ " /= expected type: " ++ show expectedType)
      pure $ Just $ CAction name val
actGen _trToGen mem (Output name expr) =
  case runExcept $ eval mem expr of
    Left err -> error $ "Expression couldn't be evaluated: " ++ show err
    Right val -> pure $ Just $ CAction name val

data Error
  = Run Run.Error
  | NoPossibleAction Transition
  -- ^ No possible action could be generated.
  deriving (Show)

actTriggerGen
  :: Map ActionName (Gen Cell)
  -> Memory
  -> Transition
  -> Gen (Either Error (CAction, (Memory, State)))
actTriggerGen trToGen mem trans@(Transition {act}) = do
  mCAct <- actGen trToGen mem act
  case mCAct of
    Nothing   ->
      pure $ Left $ NoPossibleAction trans
    Just cAct ->
      pure $ left Run $ runExcept $ (cAct,) <$> apply mem cAct trans

nextCActGen
  :: Map ActionName (Gen Cell)
  -> Memory
  -> State
  -> DataAutomaton
  -> Gen (Maybe (CAction, (Memory, State)))
nextCActGen anameToGen mem st aut = do
  ePossibleActs <- traverse (actTriggerGen anameToGen mem) (aut `transitionsFrom` st)
  case partitionEithers ePossibleActs of
    (_errs, []) -> pure Nothing -- No next action is possible.
    (_    , xs) -> Just <$> elements xs

-- | Generate a trace of the automata.
--
-- Might run till a certain trace length is achieved, or until there are no
-- actions possible.
genAutomatonTrace
  :: Map ActionName (Gen Cell)
  -> Memory
  -> DataAutomaton
  -> Gen [CAction]
genAutomatonTrace anameToGen mem aut@(DataAutomaton {start}) =
  genTrace
    (Between (Lower 0) (Upper 100))
    (mem, start)
    (\(mem', st') -> nextCActGen anameToGen mem' st' aut)
    reverse

prettySample :: Show a => Gen [a] -> IO ()
prettySample gen = sample' gen >>= pPrint

data TraceLength
  = Desired Word
  -- ^ Desired trace size. The generator will produce traces of length less than
  -- the desired trace length if the trace-elements generator can fail
  -- (returning 'Nothing').
  | Between Lower Upper
  -- ^ Produce a trace of length between lower and upper bound (both included).
  deriving (Show, Eq)

newtype Lower = Lower Word
  deriving (Show, Eq)

newtype Upper = Upper Word
  deriving (Show, Eq)

traceLengthGen :: TraceLength -> Gen Word
traceLengthGen (Desired d)                   = pure d
traceLengthGen (Between (Lower l) (Upper u)) = choose (l, u)

-- | Generic trace generator.
genTrace
  :: forall f st a
   . Alternative f
  => TraceLength
  -> st
  -- ^ Initial trace state.
  -> (st -> Gen (Maybe (a, st)))
  -- ^ Action and next state generator.
  -> (f a -> f a)
  -- ^ Post processing step. Function to be applied to the generated trace.
  -> Gen (f a)
genTrace traceLength initSt genA postF = do
  n <- traceLengthGen traceLength
  postF <$> step n empty initSt
  where
    step 0 acc _  = pure acc
    step n acc st = do
      ma <- genA st
      case ma of
        Nothing       -> step (n-1) acc              st
        Just (a, st') -> step (n-1) (pure a <|> acc) st'
        -- NOTE: when no action can be generated, we try again but decrease the
        -- trace length counter. This is what ensures that the trace generation
        -- will terminate if no actions can be generated in the given state.

--------------------------------------------------------------------------------
-- State automata actions generation
--------------------------------------------------------------------------------

automataTrace
  :: Map ActionName (Gen Cell)
  -> TraceLength
  -> LTree Memory
  -> Automata
  -> Gen [CAction]
automataTrace anameToGen traceLength mem aut =
  genTrace
    traceLength
    (mem, initialStateTree aut)
    (\(mem', st') -> genStep anameToGen (mem', st') aut)
    reverse

orElse :: Gen (Maybe a) -> Gen (Maybe a) -> Gen (Maybe a)
orElse opt0 opt1 = do
  mRes <- opt0
  case mRes of
    Nothing -> opt1
    Just _  -> pure mRes

genStep
  :: Map ActionName (Gen Cell)
  -> (LTree Memory, LTree State)
  -> Automata
  -> Gen (Maybe (CAction, (LTree Memory, LTree State)))
genStep anameToGen (Leaf mem, Leaf st) (Single aut) =
  fmap (second (Leaf *** Leaf)) <$> nextCActGen anameToGen mem st aut
genStep anameToGen (mem, st) (Sync _ aut) = genStep anameToGen (mem, st) aut
genStep anameToGen (mem0 :++ mem1, st0 :++ st1) (aut0 :|| aut1) =
  oneof [ gen0 `orElse` gen1
        , gen1 `orElse` gen0
        ]
  where
    gen0 =                        generateOn (mem0, st0, aut0) (mem1, st1, aut1)
    gen1 = fmap (second swap) <$> generateOn (mem1, st1, aut1) (mem0, st0, aut0)
    -- Try to generate an action of the first automata, and trigger the
    -- generated action on the second automata if it is a synchronizing action.
    generateOn
      :: (LTree Memory, LTree State, Automata)
      -> (LTree Memory, LTree State, Automata)
      -> Gen (Maybe (CAction, (LTree Memory, LTree State)))
    generateOn (meml, stl, autl) (memr, str, autr) = do
      mNextAct <- genStep anameToGen (meml, stl) (autl)
      case mNextAct of
        Nothing -> pure Nothing
        Just (cAct, (meml', stl')) -> do
          if actionName cAct `elem` syncs autr
          then do -- @cAct@ is synchronous in @autr@ so we need to trigger it there
            case runExcept $ automataStep autr (memr, str) cAct of
              Left _err           -> do
                pure Nothing
              Right (memr', str') -> do
                pure $ Just (cAct, (meml' :++ memr', stl' :++ str'))
          else -- @cAct@ is not synchronous in @autr@ so @autl@ can perform this action independently
            pure $ Just (cAct, (meml' :++ memr, stl' :++ str))

    swap :: (LTree Memory, LTree State) -> (LTree Memory, LTree State)
    swap (meml :++ memr, stl :++ str) = (memr :++ meml, str :++ stl)
    swap _                             = error $ "Memory and state cardinality mismatch."
genStep _ _ _ = cardinalityError

--------------------------------------------------------------------------------
-- Shrinking
--------------------------------------------------------------------------------

shrinkTrace
  :: LTree Memory
  -> Automata
  -> [CAction]
  -> [[CAction]]
shrinkTrace mem aut acts =
  filter validTrace $ shrinkList shrinkNothing acts
  where
    validTrace trace = isRight $ runExcept $ runAutomata mem aut trace

forAllTraces
  :: Testable prop
  => TraceLength
  -> Map ActionName (Gen Cell)
  -> LTree Memory
  -> Automata
  -> ([CAction] -> prop)
  -> Property
forAllTraces maxTraceSize actsGen mem aut prop =
  forAllShrinkShow cactionsGen
                   (shrinkTrace mem aut)
                   (T.unpack . pShow)
                   prop
  where
    cactionsGen = automataTrace actsGen maxTraceSize mem aut


--------------------------------------------------------------------------------
-- Invalid trace generation
--------------------------------------------------------------------------------

automataInvalidTrace
  :: Map ActionName (Gen Cell)
  -> TraceLength
  -> LTree Memory
  -> Automata
  -> Set ActionName
  -> Gen InvalidTrace
automataInvalidTrace anameToGen traceLength mem aut visibleActs = do
  genTrace
    traceLength
    (mem, initialStateTree aut)
    (\(mem', st') ->
       let genValidStep = fmap (first Right) <$> genStep anameToGen (mem', st') aut
       in
       oneof [ genValidStep
             , genInvalidStep anameToGen (mem', st') aut visibleActs `orElse` genValidStep
             ]
    )
    reverse

nextInvalidCActGen
  :: Map ActionName (Gen Cell)
  -> Memory
  -> DataAutomaton
  -> Gen (Maybe CAction)
nextInvalidCActGen anameToGen mem aut =
  oneof $ actGen anameToGen mem <$> actions aut

genInvalidStep
  :: Map ActionName (Gen Cell)
  -> (LTree Memory, LTree State)
  -> Automata
  -> Set ActionName
  -> Gen (Maybe (Either CAction CAction, (LTree Memory, LTree State)))
genInvalidStep anameToGen (mem, st) aut visibleActs = do
  mEacts <- traverse tryInvalidStep $ invalidStepGens anameToGen mem aut
  case catMaybes mEacts of
    [] -> pure Nothing
    xs -> Just <$> elements xs
  where
    tryInvalidStep cActGen = do
      mInvalidStep <- (mfilter ((`elem` visibleActs) . actionName)) <$> cActGen
      case mInvalidStep of
        Nothing   -> pure Nothing
        Just cAct ->
          if invalidActionCandidateInAutomata st aut (actionName cAct) visibleActs
          then
            case runExcept $ automataStep aut (mem, st) cAct of
              Left _err         -> pure $ Just (Left cAct, (mem, st))
              Right (mem', st') -> pure $ Just (Right cAct, (mem', st'))
              -- NOTE: here we could try again to generate an invalid action, up
              -- to a certain numbers of retries.
          else
            pure Nothing

invalidStepGens
  :: Map ActionName (Gen Cell)
  -> LTree Memory
  -> Automata
  -> [Gen (Maybe CAction)]
invalidStepGens anameToGen (Leaf mem) (Single aut) =
  [nextInvalidCActGen anameToGen mem aut]
invalidStepGens anameToGen mem (Sync _ aut) =
  invalidStepGens anameToGen mem aut
invalidStepGens anameToGen (mem0 :++ mem1) (aut0 :|| aut1) =
  invalidStepGens anameToGen mem0 aut0 ++ invalidStepGens anameToGen mem1 aut1
invalidStepGens _ _ _ = cardinalityError

-- | A visible action is candidate for being an invalid action in an automata
-- and current state if either:
--
-- - the action is not part of the visible actions of the automata, or
-- - there is a visible action that can be triggered in that state
--
-- If in the given state all the possible actions are internal, we cannot ensure
-- that the given action can be invalid, since it could be triggered after
-- performing the internal transitions in the given automata. In other words, we
-- need to be sure that the automata explicitly precludes the action from
-- occurring, by specifying which visible actions are possible.
--
invalidActionCandidate
  :: State
  -> DataAutomaton
  -> ActionName
  -> Set ActionName
  -- ^ (Globally) visible actions.
  -> Bool
invalidActionCandidate st aut anActName visibleActs
  =  anActName `notElem` fmap actionName (actions aut)
  || anActName `notElem` fmap actionName (internallyReachableActions aut st visibleActs)

-- | Determine whether an action is a candidate for an invalid action in the
-- composition of automata. See 'invalidActionCandidate'.
--
invalidActionCandidateInAutomata
  :: LTree State
  -> Automata
  -> ActionName
  -> Set ActionName
  -> Bool
invalidActionCandidateInAutomata (Leaf st) (Single aut) anActName visibleActs =
  invalidActionCandidate st aut anActName visibleActs
invalidActionCandidateInAutomata st (Sync _ aut) anActName visibleActs =
  invalidActionCandidateInAutomata st aut anActName visibleActs
invalidActionCandidateInAutomata (st0 :++ st1) (aut0 :|| aut1) anActName visibleActs
  =  invalidActionCandidateInAutomata st0 aut0 anActName visibleActs
  && invalidActionCandidateInAutomata st1 aut1 anActName visibleActs
invalidActionCandidateInAutomata _ _ _ _ = cardinalityError

-- | Necessary ingredients for a model that can generate traces.
data GeneratorModel =
  GeneratorModel
  { actionGenerators :: Map ActionName (Gen Cell)
  , runnableModel :: RunnableModel
  }
