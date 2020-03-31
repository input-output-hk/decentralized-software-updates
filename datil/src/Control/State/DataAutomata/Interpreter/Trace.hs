{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module Control.State.DataAutomata.Interpreter.Trace where

import           GHC.Exts (IsString, fromString)
import           Data.Typeable (Typeable, cast)
import           Data.List (find)
import           Data.Maybe (isJust)

import           Control.State.DataAutomata

-- | Concrete action.
data CAction
  = forall t . (Typeable t, Show t, Eq t) => CAction ActionName t

deriving instance Show CAction

instance HasActionName CAction where
  actionName (CAction name _) = name

concreteActionValue :: Typeable t => CAction -> Maybe t
concreteActionValue (CAction _ val) = cast val

instance IsString CAction where
  fromString str = CAction (fromString str) ()

type Trace = [CAction]

-- | Is there an action with the given name?
actionInTrace :: ActionName -> Trace -> Bool
actionInTrace name trace = isJust (find ((== name) . actionName) trace)

-- | Return the action that immediately precedes the action with the given name
-- if any.
actionPriorTo :: ActionName -> Trace -> Maybe CAction
actionPriorTo name trace =
  case reverse (actionsPriorTo name trace) of
    []  -> Nothing
    x:_ -> Just x

-- | Return the actions prior to the action matching the given name. If the
-- action is not found, then the resulting sub-trace is empty, ans there aren't
-- any action prior to the given name.
actionsPriorTo :: ActionName -> Trace -> Trace
actionsPriorTo name trace =
  case span ((/= name) . actionName) trace of
    (_  , [] ) -> [] -- Action name wasn't found
    ([] , _:_) -> [] -- Matching action is the first element of the list
    (xs , _:_) -> xs

-- | Return the last action that matches the given action name, if any.
--
-- TODO: add doctests
lastAction :: ActionName -> Trace -> Maybe CAction
lastAction name trace = find ((== name) . actionName) (reverse trace)

containsAction :: (HasActionName a, Foldable f, Functor f) => f a -> ActionName -> Bool
containsAction fa name = name `elem` fmap actionName fa


-- | Invalid trace
--
-- NOTE: Maybe we should call this something like "extended trace". It isn't an
-- invalid trace necessarily, but a trace extended with invalid actions ...
type InvalidTrace = [Either CAction CAction]
