{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Dummy types to be used a placeholders for real UTxO. If more concrete
-- details are needed, use the Shelley types defined in @cardano-ledger-specs@.
module Cardano.Ledger.Spec.STS.Dummy.UTxO where

import           GHC.Generics (Generic)
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           Data.Set (Set)

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, initialRules, transitionRules)


data TxIn = TxIn deriving (Eq, Show)

data TxOut = TxOut deriving (Eq, Show)

data Coin = Coin deriving (Eq, Show)

data Witness = Witness deriving (Eq, Show)

data Payload
  = Payload
    { inputs :: !(Set TxIn)
    , outputs :: ![TxOut]
    , fees :: !Coin
    }
  deriving (Eq, Show)

-- | Dummy UTxO transition system.

data UTXO

data Env = Env deriving (Eq, Show)

data St = St ()
  deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup St
  deriving Monoid via GenericMonoid St


instance STS UTXO where

  type Environment UTXO = Env

  type State UTXO = St

  type Signal UTXO = Payload

  data PredicateFailure UTXO  = UTXOFailure
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      pure $! St ()
    ]
