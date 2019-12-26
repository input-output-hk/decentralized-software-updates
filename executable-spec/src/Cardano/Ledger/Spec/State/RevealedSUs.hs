{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.RevealedSUs where

import           Data.Map.Strict (Map)

import qualified Ledger.Core as Core

import qualified Cardano.Ledger.Spec.Classes.IsSU as IsSU
import           Cardano.Ledger.Spec.Classes.Indexed (Indexed)

-- | SIP registered in the system.
newtype RevealedSUs u p = RevealedSUs (Map (IsSU.SUHash u p) (IsSU.SU u p))
  deriving newtype (Core.Relation)

deriving instance (Eq (IsSU.SUHash u p), Eq (IsSU.SU u p)) => Eq (RevealedSUs u p)
deriving instance (Show (IsSU.SUHash u p), Show (IsSU.SU u p)) => Show (RevealedSUs u p)
deriving instance (Ord (IsSU.SUHash u p), Ord (IsSU.SU u p)) => Ord (RevealedSUs u p)
deriving instance (Ord (IsSU.SUHash u p)) => Semigroup (RevealedSUs u p)
deriving instance (Ord (IsSU.SUHash u p)) => Monoid (RevealedSUs u p)
deriving instance (Ord (IsSU.SUHash u p)) => Indexed (RevealedSUs u p)
