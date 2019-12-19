{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.RevealedSUs where

import           Data.Map.Strict (Map)

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.Classes.Indexed (Indexed, (!))

-- | SIP registered in the system.
newtype RevealedSUs p u = RevealedSUs (Map (Data.SUHash p u) (Data.SU p u))
  deriving stock (Eq, Show)
  deriving newtype (Core.Relation, Semigroup, Monoid, Indexed)

-- | Return in how many slots the voting period of the specific @SIP@ will end.
-- The result is based on the voting period duration recorded in the @SIP@ metadata
-- ('Data.SIPMetadata').
votingPeriodEnd
  :: Hashable p
  => Data.SUHash p u
  -> RevealedSUs p u
  -> Core.SlotCount
votingPeriodEnd suHash sudb
  = case (sudb ! suHash) of
      Data.SUsip sip -> Data.votPeriodDuration
                        . Data.metadata
                        . Data.sipPayload $ sip
      Data.SUup up   -> Data.votPeriodDurationUP
                        . Data.metadataUP
                        . Data.upPayload $ up
