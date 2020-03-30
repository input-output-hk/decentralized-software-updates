{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Control.State.DataAutomata.Interpreter.Run where

import           Control.Arrow ((***))
import           Control.Exception (assert)
import           Control.Monad (foldM, when)
import           Control.Monad.Except (Except, catchError, runExcept,
                     throwError, withExcept, withExceptT)
import           Data.Either (partitionEithers)
import           Data.Foldable (elem)
import           Data.Sequence ((|>))
import           Data.Text (Text)
import           Data.Typeable (TypeRep, Typeable, cast, typeOf)
import           GHC.Exts (IsString, fromString)
import           Text.Pretty.Simple (pPrint)

import           Control.State.DataAutomata
import           Control.State.DataAutomata.Expr
import           Control.State.DataAutomata.Interpreter.Memory hiding (Error)
import qualified Control.State.DataAutomata.Interpreter.Memory as Memory
import           Control.State.DataAutomata.Interpreter.Trace

performUpdate :: Memory -> Update -> Except Memory.Error Memory
performUpdate mem (Var name := expr) = do
  val <- eval mem expr
  assign mem name val

performUpdates :: Memory -> [Update] -> Except Memory.Error Memory
performUpdates mem updates = foldM performUpdate mem updates

eval :: (Typeable t, Show t, Eq t) => Memory -> Expr t -> Except Memory.Error t
eval mem (VarE (Var name)) = mem .? name
eval _ (Const n) = pure n
eval mem (n :== m) = (==) <$> eval mem n <*> eval mem m
eval mem (n :< m) = (<) <$> eval mem n <*> eval mem m
eval mem (n :<= m) = (<=) <$> eval mem n <*> eval mem m
eval mem (n :* m) = (*) <$> eval mem n <*> eval mem m
eval mem (n :+ m) = (+) <$> eval mem n <*> eval mem m
eval mem (r :<> s) = (<>) <$> eval mem r <*> eval mem s
eval mem (Fapply (FunctionName fname) e) = do
  f <- (mem .? fname)
  a <- eval mem e
  pure (fapply f a)
eval mem (Fapply2 (FunctionName fname) e0 e1) = do
  f <- (mem .? fname)
  a <- eval mem e0
  b <- eval mem e1
  pure (fapply f a b)
eval mem (Fapply3 (FunctionName fname) e0 e1 e2) = do
  f <- (mem .? fname)
  a <- eval mem e0
  b <- eval mem e1
  c <- eval mem e2
  pure (fapply f a b c)
eval mem (ea :|> eas) = (|>) <$> eval mem ea <*> eval mem eas

newtype Function a b = Function { fapply :: a -> b }

instance Show (Function a b) where
  show _ = "\"<function>\""

instance Eq (Function a b) where
  _ == _ = error "Cannot compare functions"

data Error
  = NoMatch CAction Action
  | MemoryError (Memory.Error)
  | CannotTrigger CAction
  | Deadlock [(Transition, Error)]
  | NoOutputTypeMatch (Expected TypeRep) (Actual TypeRep)
  | NoOutputValueMatch (Expected String) (Actual String)
  | InterleavingParallelComposition Error Error
  deriving (Show)

isDeadlock :: Error -> Bool
isDeadlock (Deadlock _) = True
isDeadlock _            = False

run
  :: Memory
  -> DataAutomaton
  -> [CAction]
  -> Except Error (Memory, State)
run mem aut@(DataAutomaton {start}) acts =
  go (mem, start) acts
  where -- TODO: we should be able to use foldl'
    go (mem', st) []            = pure (mem', st)
    go (mem', st) (cAct: acts') = do
      (mem'', st') <- stepAutomaton aut (mem', st) cAct
      go (mem'', st') acts'

finalState :: Show err => Except err (mem, st) -> st
finalState exc = either err snd $ runExcept exc
  where
    err what = error $ "Expected Right, but got Left " ++ show what

getError :: Show b => Except (a, Error) b -> Error
getError exc = either snd err $ runExcept exc
  where
    err what = error $ "Expected Left, but got Right " ++ show what


stepAutomaton
  :: DataAutomaton
  -> (Memory, State)
  -> CAction
  -> Except Error (Memory, State)
stepAutomaton aut (mem, st) cAct =
  tryApplyOne mem cAct (aut `transitionsFrom` st)

tryApplyOne :: Memory -> CAction -> [Transition] -> Except Error (Memory, State)
tryApplyOne mem cAct transitions =
  case partitionEithers $ fmap (runExcept . apply mem cAct) transitions of
    (errors, []          ) -> throwError $ Deadlock $ zip transitions errors
    (_     , (mem', st):_) -> pure (mem', st)

liftMem :: Except Memory.Error a -> Except Error a
liftMem = withExceptT MemoryError

apply :: Memory -> CAction -> Transition -> Except Error (Memory, State)
apply mem cAct Transition {guard, act, update, to} = do
  actBinding <- getActionBinding mem cAct act
  mem' <- addActionBinding actBinding mem
  canTrigger <- liftMem $ eval mem' guard
  if canTrigger
    then do
      mem'' <- liftMem $ performUpdates mem' update
      pure (removeActionBinding actBinding mem'', to)
    else
      throwError $ CannotTrigger cAct

getActionBinding :: Memory -> CAction -> Action -> Except Error ActionBinding
getActionBinding _ cAct@(CAction aCActName value) act@(Input anActName (Var varName))
  | aCActName == anActName = pure $ Bind varName (Cell value)
  | aCActName /= anActName = throwError $ NoMatch cAct act
getActionBinding mem cAct@(CAction aCActName value) act@(Output anActName expr)
  | aCActName == anActName = do
      actualOutputValue <- liftMem $ eval mem expr
      checkEqualOutputs value actualOutputValue
      pure NoBinding
  | aCActName /= anActName = throwError $ NoMatch cAct act
getActionBinding _ cAct act = throwError $ NoMatch cAct act

checkEqualOutputs :: (Eq a, Show a, Typeable a, Typeable b) => a -> b -> Except Error ()
checkEqualOutputs expected actual =
  case cast actual of
    Nothing ->
      throwError $ NoOutputTypeMatch (Expected $ typeOf expected) (Actual $ typeOf actual)
    Just actual' ->
      when (expected /= actual')
           (throwError $ NoOutputValueMatch (Expected $ show expected) (Actual $ show actual'))

data ActionBinding
  = NoBinding
  | Bind Text Cell

addActionBinding :: ActionBinding -> Memory -> Except Error Memory
addActionBinding NoBinding mem = pure mem
addActionBinding (Bind name cell) mem = liftMem $ extend mem name cell

removeActionBinding :: ActionBinding -> Memory -> Memory
removeActionBinding NoBinding mem = mem
removeActionBinding (Bind name _) mem
  = assert (mem `contains` name)
  $ remove mem name

prettyPrint :: (Show e, Show a) => Except e a -> IO ()
prettyPrint = either pPrint pPrint . runExcept

data LTree a
  = Leaf a
  | (:++) (LTree a) (LTree a)
  deriving (Eq, Show)

instance Functor LTree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (lt :++ rt) = fmap f lt :++ fmap f rt

instance Semigroup (LTree a) where
  lt <> rt = lt :++ rt

instance IsString s => IsString (LTree s) where
  fromString s = Leaf $ fromString s

automataStep
  :: Automata
  -> (LTree Memory, LTree State)
  -> CAction
  -> Except Error (LTree Memory, LTree State)
automataStep (Single automaton) (Leaf mem, Leaf st) cAct =
  (Leaf *** Leaf) <$> stepAutomaton automaton (mem, st) cAct
automataStep (Sync _ automaton) (mem, st) cAct = automataStep automaton (mem, st) cAct
automataStep (aut0 :|| aut1) (mem0 :++ mem1, st0 :++ st1) cAct =
  case (actionName cAct `elem`) <$> [syncs aut0, syncs aut1] of
    [False, False] -> do -- the action should be performed in only one of the automata
      step0Only `catchError` withError
    [True, False]  -> do
      step0Only
    [False, True]  -> do
      step1Only
    [True, True]   -> do
      (mem0', st0') <- step0
      (mem1', st1') <- step1
      pure $! (mem0' :++ mem1', st0' :++ st1')
    _              ->
      error "All the cases should be covered by the pattern match above!"

  where
    step0 = automataStep aut0 (mem0, st0) cAct
    step1 = automataStep aut1 (mem1, st1) cAct
    step0Only = ((:++ mem1) *** (:++ st1)) <$> step0
    withError err0 = step1Only `catchError` (throwError . InterleavingParallelComposition err0)
    step1Only = ((mem0 :++) *** (st0 :++)) <$> step1
automataStep _ _ _ = cardinalityError

cardinalityError :: a
cardinalityError = error "Mismatch between memory and automata cardinality."

initialStateTree :: Automata -> LTree State
initialStateTree (Single DataAutomaton {start}) = Leaf start
initialStateTree (Sync _ aut) = initialStateTree aut
initialStateTree (aut0 :|| aut1) = initialStateTree aut0 :++ initialStateTree aut1

runAutomata
  :: LTree Memory
  -> Automata
  -> [CAction]
  -> Except (LTree State, Error) (LTree Memory, LTree State)
runAutomata mem aut acts = go (mem, initialStateTree aut) acts
  where
    go (_mem', st) []            = pure (mem, st)
    go (mem', st)  (cAct: acts') = do
      (mem'', st') <- withExcept (st,) $ automataStep aut (mem', st) cAct
      go (mem'', st') acts'

runModel
  :: RunnableModel
  -> [CAction]
  -> Except (LTree State, Error) (LTree Memory, LTree State)
runModel RunnableModel { initialMemory, automata } =
  runAutomata initialMemory automata

-- | Necessary ingredients to run a model.
--
-- TODO: we might want to define smart constructors to make sure that the memory
-- is consistent with the automata structure.
data RunnableModel =
  RunnableModel
  { initialMemory :: LTree Memory
  , automata      :: Automata
  }
