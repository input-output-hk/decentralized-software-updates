{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Utilities for specifying and running test cases.
module Util.TestCase
  ( TestCaseM
  , runTestCase
  , run
  , shouldBeRight
  , with
  , withTestCase
  , throwsError
  )
where

import           Control.Monad (unless)
import           Control.Monad.Except (ExceptT, MonadError, catchError,
                     runExceptT, throwError)
import           Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import           Control.Monad.State (MonadState, State, evalState)
import           Data.Either (isRight)
import qualified Data.Text.Lazy as Text.Lazy
import           Test.QuickCheck (Property, counterexample)
import           Test.Tasty (TestTree)
import           Test.Tasty.QuickCheck (testProperty)
import qualified Text.Pretty.Simple as Pretty

-- | The test case monad.
newtype TestCaseM e r s a =
  TestCaseM { runTestCase :: ExceptT e (ReaderT r (State s)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState s
           , MonadError e
           , MonadReader r
           )

-- | Run a test case in the given initial state.
run :: r -> s -> TestCaseM e r s a -> Either e a
run r s
  = flip evalState s
  . flip runReaderT r
  . runExceptT
  . runTestCase

shouldBeRight :: (Show a, Show e) => Either e a -> Property
shouldBeRight e =
  counterexample (Text.Lazy.unpack (Pretty.pShow e)) $ isRight e

with :: Show e => r -> s -> [(String, TestCaseM e r s ())] -> [TestTree]
with r s = fmap (withTestCase r s)

withTestCase :: Show e => r -> s -> (String, TestCaseM e r s ()) -> TestTree
withTestCase r s (testCaseLabel, testCase) =
  testProperty testCaseLabel $ shouldBeRight $ run r s testCase

-- | Catch the error thrown by the test case, and apply the given function.
--
-- If an error is not thrown, then the second given action is applied.
throwsError
  :: TestCaseM e r s a
  -- ^ Test case to run
  -> (e -> TestCaseM e r s b)
  -- ^ Function to apply when the expected error is thrown
  -> e
  -- ^ Error to throw if the test case threw no error
  -> TestCaseM e r s ()
throwsError act onError noErrorWasThrown = do
  threwError <- (act >> pure False) `catchError` ((>> pure True) . onError)
  unless threwError $
    throwError noErrorWasThrown
