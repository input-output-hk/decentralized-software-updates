{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.WhenRevealedSUs  where

import           Data.Map.Strict (Map)

import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Indexed (Indexed)
import qualified Cardano.Ledger.Spec.Classes.IsSU as IsSU

-- | Slot in which a SU was revealed.
newtype WhenRevealedSUs u p = WhenRevealedSUs (Map (IsSU.SUHash u p) Core.Slot)
  -- deriving stock (Eq, Ord, Show)
  deriving newtype (Core.Relation) -- , Semigroup, Monoid, Indexed)

deriving instance (Eq (IsSU.SUHash u p)) => Eq (WhenRevealedSUs u p)
deriving instance (Show (IsSU.SUHash u p)) => Show (WhenRevealedSUs u p)
deriving instance (Ord (IsSU.SUHash u p)) => Ord (WhenRevealedSUs u p)
deriving instance (Ord (IsSU.SUHash u p)) => Semigroup (WhenRevealedSUs u p)
deriving instance (Ord (IsSU.SUHash u p)) => Monoid (WhenRevealedSUs u p)
deriving instance (Ord (IsSU.SUHash u p)) => Indexed (WhenRevealedSUs u p)

