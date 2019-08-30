{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Update.Implementation where

import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           GHC.Generics (Generic)

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, initialRules, transitionRules)
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data


data IMPLEMENTATION

data Env = Env
  deriving (Eq, Show, Generic)

data St = St ()
  deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup St
  deriving Monoid via GenericMonoid St


instance STS IMPLEMENTATION where

  type Environment IMPLEMENTATION = Env

  type State IMPLEMENTATION = St

  type Signal IMPLEMENTATION = Data.ImplementationPayload

  data PredicateFailure IMPLEMENTATION
    = ImplementationFailure
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [ pure $! St () ]


instance HasTrace IMPLEMENTATION where

  envGen _ = pure $! Env

  sigGen _env _st = pure $! Data.ImplementationPayload
