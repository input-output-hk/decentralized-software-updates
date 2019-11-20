{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.Participants where

import           Data.Map.Strict (Map)

import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Hashable (Hash, Hashable)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     SKey, VKey)
import           Cardano.Ledger.Spec.Classes.Indexed (Indexed)


-- | The set of stakeholders (i.e., participants), identified by their signing
-- and verifying keys.
--
-- There is a one-to-one correspondence between the signing and verifying keys.
--
newtype Participants p =
  Participants (Map (Hash p (VKey p)) (SKey p))

deriving instance (Hashable p, HasSigningScheme p) => Show (Participants p)
deriving instance (Hashable p) => Core.Relation (Participants p)
deriving instance (Hashable p) => Indexed (Participants p)
