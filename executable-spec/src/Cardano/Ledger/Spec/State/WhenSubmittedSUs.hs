{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.WhenSubmittedSUs where

import           Data.Map.Strict (Map)

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Indexed (Indexed)
import qualified Cardano.Ledger.Spec.Classes.IsSUCommit as IsSUCommit

-- | Slot in which a SU was submitted.
newtype WhenSubmittedSUs u p = WhenSubmittedSUs (Map (IsSUCommit.CommitSU u p) Core.Slot)
  -- deriving stock (Eq, Ord, Show)
  deriving newtype (Core.Relation) --, Semigroup, Monoid, Indexed)

deriving instance (Eq (IsSUCommit.CommitSU u p)) => Eq (WhenSubmittedSUs u p)
deriving instance (Show (IsSUCommit.CommitSU u p)) => Show (WhenSubmittedSUs u p)
deriving instance (Ord (IsSUCommit.CommitSU u p)) => Ord (WhenSubmittedSUs u p)
deriving instance (Ord (IsSUCommit.CommitSU u p)) => Semigroup (WhenSubmittedSUs u p)
deriving instance (Ord (IsSUCommit.CommitSU u p)) => Monoid (WhenSubmittedSUs u p)
deriving instance (Ord (IsSUCommit.CommitSU u p)) => Indexed (WhenSubmittedSUs u p)

