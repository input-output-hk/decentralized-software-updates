{-# LANGUAGE DeriveGeneric #-}
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

import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)

import           Cardano.Ledger.Spec.STS.Update.Data (ideationPayload,
                     implementationPayload)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.STS.Update.Ideation (IDEATIONS)
import           Cardano.Ledger.Spec.STS.Update.Implementation (IMPLEMENTATIONS)


data UPDATE

-- | As we incorporate more phases, like UP (or IMPLEMENTATION), we will be
-- adding more components to this environment.
data Env
  = Env
    { participants :: Environment IDEATIONS
    , implementationEnv :: Environment IMPLEMENTATIONS
    }
  deriving (Eq, Show, Generic)

data St
  = St
    { ideationSt :: State IDEATIONS
    , implementationSt :: State IMPLEMENTATIONS
    }
  deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup St
  deriving Monoid via GenericMonoid St


instance STS UPDATE where

  type Environment UPDATE = Env

  type State UPDATE = St

  type Signal UPDATE = Data.UpdatePayload

  data PredicateFailure UPDATE
    = IdeationsFailure (PredicateFailure IDEATIONS)
    | ImplementationsFailure (PredicateFailure IMPLEMENTATIONS)
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC ( Env { participants, implementationEnv }
          , St { ideationSt, implementationSt }
          , Data.UpdatePayload { ideationPayload, implementationPayload }
          ) <- judgmentContext
      ideationSt' <-
        trans @IDEATIONS $
             TRC (participants, ideationSt, ideationPayload)
      implementationSt' <-
        trans @IMPLEMENTATIONS $
              TRC (implementationEnv, implementationSt, implementationPayload)
      pure $ St { ideationSt = ideationSt', implementationSt = implementationSt' }

    ]


instance Embed IDEATIONS UPDATE where
  wrapFailed = IdeationsFailure

instance Embed IMPLEMENTATIONS UPDATE where
  wrapFailed = ImplementationsFailure
