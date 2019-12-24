{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

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
import qualified Cardano.Ledger.Spec.Classes.IsSU as IsSU
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data

-- | Active SU's. The slot in the range (of the map) determines when the voting
--   period will end.
newtype ActiveSUs u = ActiveSUs (Map (IsSU.SUHasHash u) Core.Slot)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Core.Relation, Semigroup, Monoid, Indexed)
