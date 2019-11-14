{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.SubmittedSIPs where

import           Data.Map.Strict (Map)

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Indexed (Indexed)

-- | These are the SIPs that we need to generate for testing to take place.
-- From these both the commited SIP's as well as the revealed SIPs will be
-- created. This state is not part of the update protocol, it is used only for
-- SIP generation purposes.
newtype SubmittedSIPs p = SubmittedSIPs (Map (Data.Commit p) (Data.SIP p))
  deriving stock (Eq, Ord, Show)
  deriving newtype (Core.Relation, Semigroup, Monoid, Indexed)
