{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- TODO: temporary


module Cardano.Ledger.Update.TestCase where

import           Control.Arrow (first, left, (&&&))
import           Control.Monad (unless)
import           Control.Monad.Except (liftEither, throwError)
import           Control.Monad.State (get, gets, put)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Test.Tasty (TestTree)

import           Ledger.Core (BlockCount, Slot (Slot))

import           Cardano.Ledger.Spec.Classes.Hashable (hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (SKey, VKey)
import qualified Cardano.Ledger.Spec.State.ActivationState as Activation
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import qualified Cardano.Ledger.Spec.State.StakeDistribution as StakeDistribution
import           Cardano.Ledger.Spec.STS.Update (UpdatePayload)
import qualified Cardano.Ledger.Spec.STS.Update.Approval.Data as Approval.Data
import qualified Cardano.Ledger.Spec.STS.Update.Data as Update.Data
import qualified Cardano.Ledger.Spec.STS.Update.Ideation.Data as Ideation.Data

import           Cardano.Ledger.Mock (Mock, vkeyFromSkey, wordToSKey)
import           Cardano.Ledger.Update.Interface

import qualified Util.TestCase as TestCase


-- TODO: bundle the constraints somewhere else. Do not break abstraction like this.
import qualified Cardano.Ledger.Spec.STS.Update as Update
import qualified Cardano.Ledger.Update.Ideation as Ideation

instance Ideation.CanApply (Update.Env Mock) Mock

-- | A test case carries an read only test-environment, and modifies the
-- interface state as it performs the test case step. A test case can exit with
-- a 'TestError'.
type TestCase = TestAction ()

type TestAction a = TestCase.TestCaseM TestError TestCaseEnv (IState Mock) a

-- | Run a test case in the given initial state.
run :: TestCaseEnv -> TestCase -> Either TestError ()
run initialEnv = TestCase.run initialEnv (mkIState initialEnv)

with :: TestCaseEnv -> [(String, TestCase)] -> [TestTree]
with initialEnv = TestCase.with initialEnv (mkIState initialEnv)

apply
  :: UpdatePayload Mock
  -> TestCase
apply payload = do
  st  <- get
  st' <- liftEither $ left (`STSError` st) $ applyUpdatePayload payload st
  put st'

--------------------------------------------------------------------------------
-- Test case environment
--------------------------------------------------------------------------------

data TestCaseEnv =
  TestCaseEnv
  { tcK                     :: !BlockCount
    -- ^ Stability parameter, defined in terms of number of slots. This should
    -- be replaced by a parameter defined in terms of number of slots (something
    -- like @stableAfter@).
  , tcAdversarialStakeRatio :: !Float
  , tcSIPExperts            :: !(Map (SKey Mock) ParticipantVotingBehavior)
  , tcImplExperts           :: !(Map (SKey Mock) ParticipantVotingBehavior)
  , tcStakePools            :: !(Map (SKey Mock) ParticipantVotingBehavior)
  }

data ParticipantVotingBehavior = Approves | Rejects

participantApproves :: ParticipantVotingBehavior -> Bool
participantApproves Approves = True
participantApproves _        = False

getApprovers
  :: Map (SKey Mock) ParticipantVotingBehavior -> [(SKey Mock, VKey Mock)]
getApprovers participants =
  fmap (id &&& vkeyFromSkey) approvers
  where
    approvers = Map.keys $ Map.filter participantApproves participants

--------------------------------------------------------------------------------
-- Initial state elaboration from test case environment
--------------------------------------------------------------------------------

-- | Create an initial state from the given test case environment.
--
-- TODO: we need to use the implementation-experts stake distribution
-- (tcImplExperts). This requires that we use a different stake distribution in
-- the approval phase.
mkIState :: TestCaseEnv -> IState Mock
mkIState TestCaseEnv { tcK, tcAdversarialStakeRatio, tcSIPExperts, tcStakePools } =
  IState
    { iStateK                      = tcK
    , iStateMaxVotingPeriods       = 2
    , iStateStakeDist              = elaborateStakeDist tcSIPExperts
    , iStateCurrentSlot            = currentSlot
    , iStateEpochFirstSlot         = currentSlot
    , iStateSlotsPerEpoch          = 10
    , iStateR_a                    = tcAdversarialStakeRatio

    , iStateIdeation               = Ideation.initialState
    , iStateApproval               = mempty
    , iStateActivation             =
        Activation.initialState  (hash genesisImplData) genesisImplData
    , iStateStakepoolsDistribution = elaborateStakeDist tcStakePools
    }
    where
      currentSlot = Slot 0

      genesisSIPData =
        Ideation.Data.SIPData
        { Ideation.Data.url = Update.Data.URL "foo"
        , Ideation.Data.metadata =
          Ideation.Data.SIPMetadata
          { Ideation.Data.versionFrom       = (Ideation.Data.ProtVer 0, Ideation.Data.ApVer 0)
          , Ideation.Data.versionTo         = (Ideation.Data.ProtVer 0, Ideation.Data.ApVer 0)
          , Ideation.Data.impactsConsensus  = Ideation.Data.Impact
          , Ideation.Data.impactsParameters = []
          , Ideation.Data.votPeriodDuration = 0
          }
        }

      genesisImplData =
        Approval.Data.ImplementationData
        { Approval.Data.implDataSIPHash = hash genesisSIPData
        , Approval.Data.implDataVPD     = 0
        , Approval.Data.implType        =
            Approval.Data.genesisUpdateType genesisImplURL genesisImplHash
        }
        where
          genesisImplURL  = Update.Data.URL "foo"
          genesisImplHash = 0 -- TODO: for now we have not defined a type for this

-- | All the stakeholders are assigned a stake of @1@.
elaborateStakeDist
  :: Map (SKey Mock) ParticipantVotingBehavior -> StakeDistribution Mock
elaborateStakeDist
  = StakeDistribution.fromList
  . fmap (hash . vkeyFromSkey &&& const 1)
  . Map.keys

mkParticipantVotingBehavior
  :: Word
  -- ^ Id of the first participant
  -> Word
  -- ^ Number of approving keys (i.e. keys that will approve any proposal).
  -> Word
  -- ^ Number of rejecting keys (i.e. keys that will reject any proposal).
  -> Map (SKey Mock) ParticipantVotingBehavior
mkParticipantVotingBehavior firstParticipantId nrApprovingKeys nrRejectingKeys
  = Map.fromList
  $ fmap (first wordToSKey)
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
  = StateMismatch (Actual UpdateState) (Expected UpdateState) (IState Mock)
  | StateValueMismatch (Actual String) (Expected String) (IState Mock)
  | CannotMoveTo (Actual UpdateState) (Expected UpdateState) (IState Mock)
  | CannotFastForwardTo UpdateState
  -- ^ It is not possible to fast-forward to the current state (the current
  -- state was not found in the lifecycle).
  | STSError (UIError Mock) (IState Mock)
  | UnexpectedSTSError TestError
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
shouldBe :: (Show a, Eq a) => (IState Mock -> a) -> a -> TestCase
shouldBe f expectedValue = do
  actualValue <- gets f
  st          <- get
  unless (actualValue == expectedValue) $
    throwError $ StateValueMismatch (Actual $ show actualValue)
                                    (Expected $ show expectedValue)
                                    st


throwsErrorWhere :: TestAction a -> (UIError Mock -> Bool) -> TestAction ()
throwsErrorWhere act checkUIError = do
  TestCase.throwsError act checkTestError UnexpectedSuccess
  where
    checkTestError err@(STSError e _) =
      unless (checkUIError e) $
        throwError $ UnexpectedSTSError err
    checkTestError err             =
      throwError $ UnexpectedSTSError err
