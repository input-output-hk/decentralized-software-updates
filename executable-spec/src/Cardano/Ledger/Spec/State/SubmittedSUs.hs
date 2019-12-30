{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.SubmittedSUs where

import           Data.Map.Strict (Map)

import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Indexed (Indexed)
import qualified Cardano.Ledger.Spec.Classes.IsSU as IsSU
import qualified Cardano.Ledger.Spec.Classes.IsSUCommit as IsSUCommit

-- | These are the SUs that we need to generate for testing to take place.
-- From these both the commited SU's as well as the revealed SUs will be
-- created. This state is not part of the update protocol, it is used only for
-- SU generation purposes.
newtype SubmittedSUs u p = SubmittedSUs (Map (IsSUCommit.CommitSU u p) (IsSU.SU u p))
  -- deriving stock (Eq, Show)
  deriving newtype (Core.Relation) -- , Semigroup, Monoid, Indexed)

deriving instance (Eq (IsSUCommit.CommitSU u p), Eq (IsSU.SU u p)) => Eq (SubmittedSUs u p)
deriving instance (Show (IsSUCommit.CommitSU u p), Show (IsSU.SU u p)) => Show (SubmittedSUs u p)
deriving instance (Ord (IsSUCommit.CommitSU u p), Ord (IsSU.SU u p)) => Ord (SubmittedSUs u p)
deriving instance (Ord (IsSUCommit.CommitSU u p)) => Semigroup (SubmittedSUs u p)
deriving instance (Ord (IsSUCommit.CommitSU u p)) => Monoid (SubmittedSUs u p)
deriving instance (Ord (IsSUCommit.CommitSU u p)) => Indexed (SubmittedSUs u p)

