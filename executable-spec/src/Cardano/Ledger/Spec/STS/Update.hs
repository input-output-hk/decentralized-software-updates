{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Update where

import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           GHC.Generics (Generic)

import           Control.State.Transition.Trace (traceSignals, TraceOrder (OldestFirst))
import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen, genTrace)
import           Data.AbstractSize (HasTypeReps)

import Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import           Cardano.Ledger.Spec.STS.Update.Data (IdeationPayload, ImplementationPayload)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.STS.Update.Ideation (IDEATION)
import           Cardano.Ledger.Spec.STS.Update.Implementation (IMPLEMENTATION)


data UPDATE


-- | As we incorporate more phases, like UP (or IMPLEMENTATION), we will be
-- adding more components to this environment.
data Env
  = Env
    { participants :: Environment IDEATION
    , implementationEnv :: Environment IMPLEMENTATION
    }
  deriving (Eq, Show, Generic)


data St
  = St
    { ideationSt :: State IDEATION
    , implementationSt :: State IMPLEMENTATION
    }
  deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup St
  deriving Monoid via GenericMonoid St


data UpdatePayload
  = Ideation IdeationPayload
  | Implementation ImplementationPayload
  deriving (Eq, Show, Generic, HasTypeReps)

instance Sized UpdatePayload where
  costsList _
    =  costsList (undefined :: IdeationPayload)
    ++ costsList (undefined :: ImplementationPayload)

instance STS UPDATE where

  type Environment UPDATE = Env

  type State UPDATE = St

  type Signal UPDATE = UpdatePayload

  data PredicateFailure UPDATE
    = IdeationsFailure (PredicateFailure IDEATION)
    | ImplementationsFailure (PredicateFailure IMPLEMENTATION)
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC ( Env { participants, implementationEnv }
          , St { ideationSt, implementationSt }
          , update
          ) <- judgmentContext

      case update of
        Ideation ideationPayload ->
          do
            ideationSt' <- trans @IDEATION $
              TRC (participants, ideationSt, ideationPayload)
            pure $ St { ideationSt = ideationSt' }
        Implementation implementationPayload ->
          do
            implementationSt' <-
              trans @IMPLEMENTATION $
              TRC (implementationEnv, implementationSt, implementationPayload)
            pure $ St { implementationSt = implementationSt' }

    ]


instance Embed IDEATION UPDATE where
  wrapFailed = IdeationsFailure

instance Embed IMPLEMENTATION UPDATE where
  wrapFailed = ImplementationsFailure

data UPDATES

instance STS UPDATES where

  type Environment UPDATES = Environment UPDATE

  type State UPDATES = State UPDATE

  type Signal UPDATES = [Signal UPDATE]

  data PredicateFailure UPDATES = UpdateFailure (PredicateFailure UPDATE)
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC (env, st, updates) <- judgmentContext
      case updates of
        [] -> pure $! st
        (update:updates') ->
          do
            st' <- trans @UPDATE $ TRC (env, st, update)
            trans @UPDATES $ TRC (env, st', updates')
    ]


instance Embed UPDATE UPDATES where
  wrapFailed = UpdateFailure

--------------------------------------------------------------------------------
-- Trace generators
--------------------------------------------------------------------------------

instance HasTrace UPDATES where

  envGen traceLength = envGen @UPDATE traceLength

  sigGen env st
    =   traceSignals OldestFirst
    <$> genTrace @UPDATE 10 env st (sigGen @UPDATE)
    -- TODO: we need to determine what is a realistic number of update
    -- transactions to be expected in a block.


instance HasTrace UPDATE where

  envGen traceLength =
    Env <$> envGen @IDEATION traceLength <*> envGen @IMPLEMENTATION traceLength

  sigGen Env { participants } St { ideationSt } =
    -- For now we generate ideation payload only.
    Ideation <$> sigGen @IDEATION participants ideationSt
