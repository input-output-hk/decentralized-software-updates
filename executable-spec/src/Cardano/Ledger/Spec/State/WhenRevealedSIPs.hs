{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.WhenRevealedSIPs  where

import           Data.Map.Strict (Map)

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Indexed (Indexed)

-- | Slot in which a SIP was revealed.
newtype WhenRevealedSIPs p = WhenRevealedSIPs (Map (Data.SIPHash p) Core.Slot)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Core.Relation, Semigroup, Monoid, Indexed)
