{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.Participants where

import           GHC.Exts (IsList, toList)

import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     SKey, VKey)

-- | The set of stakeholders (i.e., participants), identified by their signing
-- and verifying keys.
--
-- There should be a one-to-one correspondence between the signing and verifying
-- keys.
--
-- TODO: introduce a smart constructor to check uniqueness of signing and
-- verification keys.
--
newtype Participants p =
  Participants [(VKey p, SKey p)]
  deriving newtype (IsList)

deriving instance (Hashable p, HasSigningScheme p) => Show (Participants p)

vkeyHashes
  :: ( Hashable p
     , HasHash p (VKey p)
     )
  => Participants p -> [Hash p (VKey p)]
vkeyHashes = fmap (hash . fst) . toList
