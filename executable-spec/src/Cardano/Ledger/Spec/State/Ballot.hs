{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.Ballot where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Cardano.Ledger.Spec.STS.Update.Data (confidence, votedsipHash,
                     voter)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Ledger.Core ((⨃))
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.Classes.Indexed (Indexed, (!))
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)

-- | Register of votes for each SIP and voter.
newtype Ballot p = Ballot (Map (Data.SIPHash p) (SIPBallot p))
  deriving stock (Eq, Ord, Show)
  deriving newtype (Core.Relation, Semigroup, Monoid, Indexed)

-- | Ballot associated to a given SIP.
newtype SIPBallot p = SIPBallot (Map Core.VKey Data.Confidence)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Core.Relation, Semigroup, Monoid, Indexed)

ballotFor :: Hashable p => Data.SIPHash p -> Ballot p -> SIPBallot p
ballotFor sipHash (Ballot ballotMap) =
  case Map.lookup sipHash ballotMap of
    Nothing -> mempty
    Just ballotForSip -> ballotForSip

-- | Given a set of votes for an SIP, compute the voting result.
addVotes :: StakeDistribution p -> Data.VotingResult -> SIPBallot p -> Data.VotingResult
addVotes stakeDistribution votingResult (SIPBallot ballot) =
  Map.foldrWithKey'
  (\vkey confidence votingResult'
    -> Data.addVote (stakeDistribution ! vkey) confidence votingResult'
  )
  votingResult
  ballot

-- | Update the ballot with the given vote.
--
-- Note that if a voter already cast a vote (according to the given ballot),
-- then the old vote is replaced with the new one.
updateBallot :: Hashable p => Ballot p -> Data.VoteForSIP p -> Ballot p
updateBallot
  (Ballot ballot)
  Data.VoteForSIP { votedsipHash, voter, confidence }
  = Ballot
  $ Map.insertWith
      (\_newMap oldMap -> oldMap ⨃ [(voter, confidence)])
      votedsipHash
      mempty
      ballot
