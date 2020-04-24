{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.ApprovedSIPs where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.Classes.Indexed (Indexed)
import qualified Cardano.Ledger.Spec.STS.Update.Ideation.Data as Data

newtype ApprovedSIPs p = ApprovedSIPs (Map (Data.SIPHash p) Core.Slot)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Core.Relation, Semigroup, Monoid, Indexed)

registerApproval
  :: Hashable p => Data.SIPHash p -> Core.Slot -> ApprovedSIPs p -> ApprovedSIPs p
registerApproval sipHash whenApproved (ApprovedSIPs sipHashes) =
  ApprovedSIPs $ Map.insert sipHash whenApproved sipHashes

isSIPApproved
  :: Hashable p => Data.SIPHash p -> ApprovedSIPs p -> Bool
isSIPApproved sipHash (ApprovedSIPs sipHashes) = Map.member sipHash sipHashes

whenSIPApproved
  :: Hashable p => Data.SIPHash p -> ApprovedSIPs p -> Maybe Core.Slot
whenSIPApproved sipHash (ApprovedSIPs sipHashes) = Map.lookup sipHash sipHashes
