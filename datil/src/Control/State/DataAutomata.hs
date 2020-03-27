{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Control.State.DataAutomata where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           GHC.Exts (IsString)
import           Data.Set (Set, (\\))
import qualified Data.Set as Set

import           Control.State.DataAutomata.Expr


-- | A data automata consists of a set of states, and transitions between those
-- states.
data DataAutomaton =
  DataAutomaton
  { start       :: State
  , transitions :: [Transition]
  } deriving (Show)

-- | A state is simply a label.
--
newtype State = State Text
  deriving (IsString, Eq, Ord, Show)

data Transition =
  Transition
  { from   :: State
  , guard  :: Expr Bool
  , act    :: Action
  , update :: [Update]
  , to     :: State
  } deriving (Show)

-- | Instances of this class combined with function '(.-->)' provide a
-- convenient way to specifying transitions.
--
class TransitionPreBuilder a where
  (.--) :: State -> a -> (State -> Transition)

infixl 2 .--

instance TransitionPreBuilder (Expr Bool, Action, [Update]) where
  fromSt .-- (someGuard, someAct, someUpdates) =
    \toSt -> Transition
             { from = fromSt
             , guard = someGuard
             , act = someAct
             , update = someUpdates
             , to = toSt
             }

instance TransitionPreBuilder (Expr Bool, Action, Update) where
  fromSt .-- (someGuard, someAct, someUpdate) = fromSt .-- (someGuard, someAct, [someUpdate])

instance TransitionPreBuilder (Expr Bool, Action) where
  fromSt .-- (someGuard, someAct) = fromSt .-- (someGuard, someAct, [] :: [Update])

instance TransitionPreBuilder (Action, [Update]) where
  fromSt .-- (someAct, someUpdates) = fromSt .-- (Const True, someAct, someUpdates)

instance TransitionPreBuilder (Action, Update) where
  fromSt .-- (someAct, someUpdate) = fromSt .-- (someAct, [someUpdate])

instance TransitionPreBuilder Action where
  fromSt .-- someAct = fromSt .-- (someAct, ([] :: [Update]))

(.-->)  :: (State -> Transition) -> State -> Transition
f .--> target = f target

infixl 2 .-->

-- | Actions are of two type: input or output. Its meaning depends on the
-- automata interpreter.
data Action
  = forall t . Typeable t                 => Input ActionName (Var t)
  | forall t . (Typeable t, Show t, Eq t) => Output ActionName (Expr t)

newtype ActionName = ActionName { unActionName :: Text}
  deriving (Eq, Ord, Show, IsString)

class HasActionName a where
  actionName :: a -> ActionName

instance HasActionName Action where
  actionName (Input name _) = name
  actionName (Output name _) = name

instance (HasActionName a, HasActionName b) => HasActionName (Either a b) where
  actionName = either actionName actionName

instance Show Action where
  show (Input name var) = "Input " ++ show name  ++ " " ++ show var
  show (Output name expr) = "Output " ++ show name ++ " " ++ show expr

(#?) :: Typeable t => ActionName -> Var t -> Action
(#?) = Input

infixl 3 #?

(.@) :: (Show n) => Text -> n -> ActionName
namePrefix .@ i = ActionName $ namePrefix <> "_" <> T.pack (show i)

infixl 3 .@

-- | Create an action that outputs the given value.
(#!)
  :: ( Typeable t
     , Show t
     , Eq t
     ) => ActionName -> t -> Action
name #! val = Output name (Const val)

infixl 3 #!

(#!!) :: (Typeable t, Show t, Eq t) => ActionName -> Expr t -> Action
name #!! expr = Output name expr

infixl 3 #!!

(.@!) :: (Show n) => Text -> n -> Action
name .@! n = Output (name .@ n) (Const ())


data Update
  = forall t . (Show t, Typeable t, Eq t) => Var t := Expr t

infixl 3 :=

(.=) :: forall a b . (ToExpr a b, Eq b) => Var b -> a -> Update
var .= a = var := toExpr a

infixl 3 .=

deriving instance Show Update

transitionsFrom :: DataAutomaton -> State -> [Transition]
transitionsFrom DataAutomaton { transitions } st = filter ((st ==) . from) transitions

actionsFrom :: DataAutomaton -> State -> [Action]
actionsFrom aut st = act <$> transitionsFrom aut st

statesFromVia :: DataAutomaton -> State -> Set ActionName -> [State]
statesFromVia aut st internalActions
  = fmap to $ filter ((`elem` internalActions) . actionName . act) $ transitionsFrom aut st

-- | Get all actions of the automata.
actions :: DataAutomaton -> [Action]
actions = fmap act . transitions

actionNamesSet :: DataAutomaton -> Set ActionName
actionNamesSet = Set.fromList . fmap actionName . actions

-- | Return all the actions that can be reached __in one step or more__ from a
-- given state, via internal actions.
--
-- The set of internal actions is indirectly determined via the given set of
-- visible actions.
internallyReachableActions
  :: DataAutomaton
  -> State
  -> Set ActionName
  -- ^ Visible actions of the automaton.
  -> [Action]
internallyReachableActions aut st visibleActions = go toVisit0 mempty mempty
  where
    toVisit0 = statesFromVia aut st internalActions

    go []        _visited reachableActions = reachableActions
    go (st':sts) visited  reachableActions =
      go (toVisit ++ sts) visited' (reachableActions <> actionsFrom aut st')
      where
        visited' = Set.insert st' visited
        toVisit = filter (`notElem` visited') $ statesFromVia aut st' internalActions

    internalActions = actionNamesSet aut \\ visibleActions

--------------------------------------------------------------------------------
-- Parallel composition
--------------------------------------------------------------------------------

data Automata
  = Single DataAutomaton
  | Sync (Set ActionName) Automata
  | (:||) Automata Automata

syncs :: Automata -> Set ActionName
syncs (Single _) = mempty
syncs (Sync acts aut) = acts <> syncs aut
syncs (aut0 :|| aut1) = syncs aut0 <> syncs aut1

instance Semigroup Automata where
  aut0 <> aut1 = aut0 :|| aut1
