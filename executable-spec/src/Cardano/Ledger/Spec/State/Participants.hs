{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.Participants where

import           Data.Bimap (Bimap)

import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Indexed (Indexed)


-- | The set of stakeholders (i.e., participants), identified by their signing
-- and verifying keys.
--
-- There is a one-to-one correspondence between the signing and verifying keys.
--
newtype Participants p =
  -- We use of 'Bimap' to ensure the one to one correspondence between signing
  -- and verifying keys.
  Participants (Bimap Core.VKey Core.SKey)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Core.Relation, Indexed)
