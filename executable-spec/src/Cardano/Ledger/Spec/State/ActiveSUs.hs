{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- The Core.Relation instance requires an 'Ord' constraint on the domain and
-- range of the relation. Both 'Data.SIPHash' and 'Core.Slot' have 'Ord'
-- instances. Thus the 'Ord' constraint is redundant. However we need this to be
-- able to use the newtype deriving mechanism.
-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.ActiveSUs where

import           Data.Map.Strict (Map)


import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Indexed (Indexed)
import qualified Cardano.Ledger.Spec.Classes.Indexed2 as Indexed2
import qualified Cardano.Ledger.Spec.Classes.IsSU as IsSU

-- | Active SU's. The slot in the range (of the map) determines when the voting
--   period will end.
newtype ActiveSUs u p = ActiveSUs (Map (IsSU.SUHash u p) Core.Slot)
  deriving newtype (Core.Relation)

deriving instance (Eq (IsSU.SUHash u p)) => Eq (ActiveSUs u p)
deriving instance (Ord (IsSU.SUHash u p)) => Ord (ActiveSUs u p)
deriving instance (Show (IsSU.SUHash u p)) => Show (ActiveSUs u p)
deriving instance (Ord (IsSU.SUHash u p)) => Semigroup (ActiveSUs u p)
deriving instance (Ord (IsSU.SUHash u p)) => Monoid (ActiveSUs u p)
deriving instance (Ord (IsSU.SUHash u p)) => Indexed (ActiveSUs u p)
deriving instance (Ord (IsSU.SUHash u p)) => Indexed2.Indexed (ActiveSUs u p)
