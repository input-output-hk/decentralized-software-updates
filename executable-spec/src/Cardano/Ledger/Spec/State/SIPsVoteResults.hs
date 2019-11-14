{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.SIPsVoteResults where

import           Control.Arrow ((&&&))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Word (Word8)

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.Classes.Indexed (Indexed)


newtype SIPsVoteResults p = SIPsVoteResults (Map (Data.SIPHash p) Data.VotingResult)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Core.Relation, Semigroup, Monoid, Indexed)

-- | Get the re-voting counters associated with the given SIP hash. If the given
-- SIP hash is not in the voting results then zero is returned for each
-- counter.
getRevotingCounters
  :: Hashable p
  => Data.SIPHash p
  -> SIPsVoteResults p
  -> (Word8, Word8)
getRevotingCounters sipHash (SIPsVoteResults sipHashesToResult) =
  Map.findWithDefault (0, 0) sipHash (fmap (Data.rvNoQuorum &&& Data.rvNoMajority) sipHashesToResult)
