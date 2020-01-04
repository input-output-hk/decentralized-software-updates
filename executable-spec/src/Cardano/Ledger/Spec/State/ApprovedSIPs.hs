{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.State.ApprovedSIPs where

import           Data.Set as Set (Set)
import qualified Data.Set as Set

import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data


newtype ApprovedSIPs p = ApprovedSIPs (Set (Data.SIPHash p))
  deriving stock (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid)

registerApproval
  :: Hashable p => Data.SIPHash p -> ApprovedSIPs p -> ApprovedSIPs p
registerApproval sipHash (ApprovedSIPs sipHashes) =
  ApprovedSIPs $ Set.insert sipHash sipHashes

isSIPApproved
  :: Hashable p => Data.SIPHash p -> ApprovedSIPs p -> Bool
isSIPApproved sipHash (ApprovedSIPs sipHashes) = Set.member sipHash sipHashes

getApprovedSIPs :: ApprovedSIPs p -> Set (Data.SIPHash p)
getApprovedSIPs (ApprovedSIPs sips) = sips
