{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Update.Implementation where

--import           Data.Map.Strict (Map)
--import qualified Data.Map.Strict as Map
import           Data.Set as Set (Set)
import qualified Data.Set as Set

import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           GHC.Generics (Generic)

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, initialRules, transitionRules)
import           Ledger.Core (Slot)

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data


data IMPLEMENTATION hashAlgo

data Env hashAlgo =
  Env { currentSlot :: !Slot
        -- ^ The current slot in the blockchain system
      , apprvsips :: !(Set (Data.SIPHash hashAlgo))
        -- ^ Set of approved SIPs
      }
  deriving (Eq, Show, Generic)

data St = St ()
  deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup St
  deriving Monoid via GenericMonoid St


instance STS (IMPLEMENTATION hashAlgo) where

  type Environment (IMPLEMENTATION hashAlgo) = Env hashAlgo

  type State (IMPLEMENTATION hashAlgo) = St

  type Signal (IMPLEMENTATION hashAlgo) = Data.ImplementationPayload

  data PredicateFailure (IMPLEMENTATION hashAlgo)
    = ImplementationFailure
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [ pure $! St () ]
