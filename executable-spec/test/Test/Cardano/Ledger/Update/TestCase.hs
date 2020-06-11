{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- TODO: see TODO note in 'HasStakeDistribution' instances of 'IState'.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.Update.TestCase where

import           Control.Arrow (first, left, (&&&))
import           Control.Monad (unless)
import           Control.Monad.Except (liftEither, throwError)
import           Control.Monad.State (get, gets, put)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Word (Word64)
import           Test.Tasty (TestTree)

import           Cardano.Slotting.Block (BlockNo)

import           Cardano.Ledger.Update.Env.HasStakeDistribution
                     (HasStakeDistribution, stakeDistribution)
import           Cardano.Ledger.Update.Env.StakeDistribution (StakeDistribution)
import qualified Cardano.Ledger.Update.Env.StakeDistribution as StakeDistribution

import           Test.Cardano.Ledger.Update.Interface

import qualified Util.TestCase as TestCase

import qualified Cardano.Ledger.Update as Update

import           Cardano.Ledger.Update.Proposal hiding (Implementation)

import           Test.Cardano.Ledger.Update.Data


-- | A test case carries an read only test-environment, and modifies the
-- interface state as it performs the test case step. A test case can exit with
-- a 'TestError'.
type TestCase = TestAction ()

type MockUIState = IState MockSIP MockImpl
type MockUIError = UIError MockSIP MockImpl

-- TODO: This is an orphan instance. We have to consider whether it makes sense
-- to avoid it. This might require making IState non-polymorphic on the sip's
-- and implementation payload, which might make sense.
instance HasStakeDistribution (IState MockSIP MockImpl) (VoterId MockSIP) where
  stakeDistribution = iStateSIPStakeDist

instance HasStakeDistribution (IState MockSIP MockImpl) (VoterId MockImpl) where
  stakeDistribution = iStateImplStakeDist

type TestAction a =
  TestCase.TestCaseM TestError TestCaseEnv MockUIState a

-- | Run a test case in the given initial state.
run :: TestCaseEnv -> TestCase -> Either TestError ()
run initialEnv = TestCase.run initialEnv (mkIState initialEnv)

with :: TestCaseEnv -> [(String, TestCase)] -> [TestTree]
with initialEnv = TestCase.with initialEnv (mkIState initialEnv)

apply
  :: Update.Payload MockSIP MockImpl
  -> TestCase
apply payload = do
  st  <- get
  st' <- liftEither $ left (`UpdateError` st) $ applyUpdate payload st
  put st'

--------------------------------------------------------------------------------
-- Test case environment
--------------------------------------------------------------------------------

data TestCaseEnv =
  TestCaseEnv
  { tcK                     :: !BlockNo
    -- ^ Stability parameter, defined in terms of number of slots. This should
    -- be replaced by a parameter defined in terms of number of slots (something
    -- like @stableAfter@).
  , tcAdversarialStakeRatio :: !Float
  , tcSIPExperts            :: !(Map (Voter MockSIP) VotingBehavior)
  , tcImplExperts           :: !(Map (Voter MockImpl) VotingBehavior)
  , tcStakePools            :: !(Map (Endorser (Protocol MockImpl)) VotingBehavior)
  }

data VotingBehavior = Approves | Rejects

participantApproves :: VotingBehavior -> Bool
participantApproves Approves = True
participantApproves _        = False

getApprovers
  :: Map d VotingBehavior -> [d]
getApprovers participants =
  Map.keys $ Map.filter participantApproves participants

--------------------------------------------------------------------------------
-- Initial state elaboration from test case environment
--------------------------------------------------------------------------------

-- | Create an initial state from the given test case environment.
--
mkIState :: TestCaseEnv -> MockUIState
mkIState env =
  IState
    { iStateK                      = tcK env
    , iStateMaxVotingPeriods       = 2
    , iStateSIPStakeDist           = elaborateStakeDist (tcSIPExperts env)
    , iStateImplStakeDist          = elaborateStakeDist (tcImplExperts env)
    , iStateCurrentSlot            = currentSlot
    , iStateEpochFirstSlot         = currentSlot
    , iStateSlotsPerEpoch          = 10
    , iStateR_a                    = tcAdversarialStakeRatio env

    , updateSt                     = Update.initialState genesisProtocol

    , iStateStakepoolsDistribution = elaborateStakeDist (tcStakePools env)
    }
    where
      currentSlot     =  0
      genesisProtocol =
        MockProtocol
        { mpProtocolId        = ProtocolId 0
        , mpProtocolVersion   = Version 0
        , mpSupersedesId      = ProtocolId 0
        , mpSupersedesVersion = Version 0
        }

-- | All the stakeholders are assigned a stake of @1@.
elaborateStakeDist
  :: Identifiable d => Map d VotingBehavior -> StakeDistribution (Id d)
elaborateStakeDist
  = StakeDistribution.fromList
  . fmap (_id &&& const 1)
  . Map.keys

-- | Create a map from a certain participant type to its voting behavior.
mkVotingBehavior
  :: Ord participant
  => Word64
  -- ^ Id of the first participant
  -> Word64
  -- ^ Number of approving keys (i.e. keys that will approve any proposal).
  -> Word64
  -- ^ Number of rejecting keys (i.e. keys that will reject any proposal).
  -> (Word64 -> participant)
  -- ^ Participant elaboration function.
  -> Map participant VotingBehavior
mkVotingBehavior firstParticipantId nrApprovingKeys nrRejectingKeys elaborate
  = Map.fromList
  $ fmap (first elaborate)
  $  zip [firstParticipantId .. (n - 1) ]
         (repeat Approves)
  ++ zip [n .. n + nrRejectingKeys - 1]
         (repeat Rejects)
  where
    n = firstParticipantId + nrApprovingKeys

--------------------------------------------------------------------------------
-- Test case errors
--------------------------------------------------------------------------------

data TestError
  = StateMismatch (Actual UpdateState) (Expected UpdateState) MockUIState
  | StateValueMismatch (Actual String) (Expected String) MockUIState
  | CannotMoveTo (Actual UpdateState) (Expected UpdateState) MockUIState
  | CannotFastForwardTo UpdateState
  -- ^ It is not possible to fast-forward to the current state (the current
  -- state was not found in the lifecycle).
  | UpdateError MockUIError MockUIState
  | UnexpectedUpdateError TestError
  -- ^ A test case can declare certain errors to be expected. This error can be
  -- thrown when an unexpected error occurs.
  | UnexpectedSuccess
  deriving (Show)

newtype Actual a = Actual a
  deriving (Show, Eq)

newtype Expected a = Expected a
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Test case assertions
--------------------------------------------------------------------------------

-- | Assert that the state component has the expected value.
shouldBe :: (Show a, Eq a) => (MockUIState -> a) -> a -> TestCase
shouldBe f expectedValue = do
  actualValue <- gets f
  st          <- get
  unless (actualValue == expectedValue) $
    throwError $ StateValueMismatch (Actual $ show actualValue)
                                    (Expected $ show expectedValue)
                                    st


throwsErrorWhere :: TestAction a -> (MockUIError -> Bool) -> TestAction ()
throwsErrorWhere act checkUIError = do
  TestCase.throwsError act checkTestError UnexpectedSuccess
  where
    checkTestError err@(UpdateError e _) =
      unless (checkUIError e) $
        throwError $ UnexpectedUpdateError err
    checkTestError err             =
      throwError $ UnexpectedUpdateError err
