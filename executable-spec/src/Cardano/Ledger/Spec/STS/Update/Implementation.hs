{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Update.Implementation where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           GHC.Generics (Generic)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, initialRules, transitionRules)
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)
import           Ledger.Core (Slot (Slot))

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data


data IMPLEMENTATION hashAlgo

data Env hashAlgo =
  Env { currentSlot :: !Slot
        -- ^ The current slot in the blockchain system
      , vresips :: !(Map (Data.SIPHash hashAlgo) Data.VotingResult)
        -- ^ Records the current voting result for each SIP
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


instance HasTrace (IMPLEMENTATION hashAlgo) where

  envGen _ =
    Env <$> currentSlotGen <*> vresipsGen
    where
      currentSlotGen = Slot <$> Gen.integral (Range.constant 0 100)
      vresipsGen = pure $ Map.empty

  sigGen _env _st = pure $! Data.ImplementationPayload
