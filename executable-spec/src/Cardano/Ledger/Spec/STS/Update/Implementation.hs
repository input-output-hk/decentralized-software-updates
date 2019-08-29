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

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data


data IMPLEMENTATIONS

data Env = Env
  deriving (Eq, Show, Generic)

data St = St ()
  deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup St
  deriving Monoid via GenericMonoid St


instance STS IMPLEMENTATIONS where

  type Environment IMPLEMENTATIONS = Env
  -- TODO: this should become
  --
  -- > Environment IMPLEMENTATION

  type State IMPLEMENTATIONS = St
  -- TODO: this should become
  --
  -- > State IMPLEMENTATION

  type Signal IMPLEMENTATIONS = [Data.ImplementationPayload]
    -- TODO: this should become:
    --
    -- > [Signal IMPLEMENTATION]

  data PredicateFailure IMPLEMENTATIONS
    = ImplementationFailure
    deriving (Eq, Show)

  initialRules = []

  transitionRules = []

data IMPLEMENTATION
