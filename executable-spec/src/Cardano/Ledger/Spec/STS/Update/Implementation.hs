{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Cardano.Ledger.Spec.STS.Update.Implementation where

import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           GHC.Generics (Generic)
import           Data.AbstractSize (HasTypeReps)
import           Data.Typeable (typeOf)

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, initialRules, transitionRules)
import           Ledger.Core (Slot)

import           Cardano.Ledger.Spec.State.ApprovedSIPs (ApprovedSIPs)
import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)


data ImplementationPayload = ImplementationPayload
  deriving (Eq, Show, Generic, HasTypeReps)

instance Sized ImplementationPayload where
  costsList implementationPayload = [(typeOf implementationPayload, 10)]

data IMPLEMENTATION p

data Env p =
  Env { currentSlot :: !Slot
        -- ^ The current slot in the blockchain system
      , apprvsips :: !(ApprovedSIPs p)
        -- ^ Set of approved SIPs
      }
  deriving (Eq, Ord, Show, Generic)

data St = St ()
  deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup St
  deriving Monoid via GenericMonoid St


instance STS (IMPLEMENTATION p) where

  type Environment (IMPLEMENTATION p) = Env p

  type State (IMPLEMENTATION p) = St

  type Signal (IMPLEMENTATION p) = ImplementationPayload

  data PredicateFailure (IMPLEMENTATION p)
    = ImplementationFailure
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [ pure $! St () ]
