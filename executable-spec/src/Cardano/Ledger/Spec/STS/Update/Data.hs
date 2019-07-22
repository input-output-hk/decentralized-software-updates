{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Cardano.Ledger.Spec.STS.Update.Data where

import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           Data.Set (Set)
import           GHC.Generics (Generic)

import qualified Ledger.Core as Core

-- | System improvement proposal
data SIP =
  SIP { author :: Core.VKey
        -- ^ Who submitted the proposal
      }
  deriving (Eq, Show, Ord)

-- | Ideation phase state
data State
  = State
    { submittedSIPs :: !(Set SIP)
    , revealedSIPs :: !(Set SIP)
    }
  deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup State
  deriving Monoid via GenericMonoid State

-- | Ideation signals.
data Signal
  = Submit SIP
  | Reveal SIP
  deriving (Eq, Ord, Show)
