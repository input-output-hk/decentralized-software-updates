{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.RevealedSIPs where

import           Data.Map.Strict (Map)
import           Data.Maybe (fromMaybe)

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.Classes.Indexed (Indexed, (!))

-- | SIP registered in the system.
newtype RevealedSIPs p = RevealedSIPs (Map (Data.SIPHash p) (Data.SIP p))
  deriving stock (Eq, Show)
  deriving newtype (Core.Relation, Semigroup, Monoid, Indexed)

-- | Return in how many slots the voting period of the specific @SIP@ will end.
-- The result is based on the voting period duration recorded in the @SIP@ metadata
-- ('Data.SIPMetadata').
votingPeriodEnd
  :: Hashable p
  => Data.SIPHash p
  -> RevealedSIPs p
  -> Core.SlotCount
votingPeriodEnd sipHash sipdb
  = Data.votPeriodDuration
  . Data.metadata
  . Data.sipPayload
  $ sip
  where
    sip = fromMaybe err $ sipdb ! sipHash
    err = error $ "Could not find SIP with the given hash" ++ show sipHash
