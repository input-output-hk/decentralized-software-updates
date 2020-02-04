{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.WhenSubmittedSIPs where

import           Data.Map.Strict (Map)

import           Cardano.Ledger.Spec.STS.Update.Data.Commit (Commit)
import qualified Cardano.Ledger.Spec.STS.Update.Ideation.Data as Data
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Indexed (Indexed)

-- | Slot in which a SIP was submitted.
newtype WhenSubmittedSIPs p = WhenSubmittedSIPs (Map (Commit p (Data.SIP p)) Core.Slot)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Core.Relation, Semigroup, Monoid, Indexed)
