{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.BallotSUs where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Ledger.Core ((⨃))
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     VKey)
import           Cardano.Ledger.Spec.Classes.Indexed (Indexed, (!))
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import           Cardano.Ledger.Spec.STS.Update.Data (confidenceSIP, votedSIPHash,
                     voterSIP)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Cardano.Ledger.Spec.Classes.IsSU as IsSU
import qualified Cardano.Ledger.Spec.Classes.IsVoteForSU as IsVote

-- | Register of votes for each SU and voter.
newtype BallotSUs u p = BallotSUs (Map (IsSU.SUHash u p) (SUBallot p))
  -- deriving stock (Eq, Show)
  deriving newtype (Core.Relation) -- , Semigroup, Monoid, Indexed)

deriving instance (Hashable p, Eq (IsSU.SUHash u p)) => Eq (BallotSUs u p)
deriving instance (Hashable p, Show (IsSU.SUHash u p)) => Show (BallotSUs u p)
deriving instance ( Hashable p
                  , Ord (IsSU.SUHash u p)
                  , Ord (SUBallot p)
                  ) => Ord (BallotSUs u p)
deriving instance (Hashable p, Ord (IsSU.SUHash u p)) => Semigroup (BallotSUs u p)
deriving instance (Hashable p, Ord (IsSU.SUHash u p)) => Monoid (BallotSUs u p)
deriving instance (Hashable p, Ord (IsSU.SUHash u p)) => Indexed (BallotSUs u p)


-- | Ballot associated to a given SU.
newtype SUBallot p = SUBallot (Map (Hash p (VKey p)) Data.Confidence)

deriving instance Hashable p => Show (SUBallot p)
deriving instance Hashable p => Eq (SUBallot p)
deriving instance Hashable p => Core.Relation (SUBallot p)
deriving instance Hashable p => Semigroup (SUBallot p)
deriving instance Hashable p => Monoid (SUBallot p)
deriving instance Hashable p => Indexed (SUBallot p)

ballotFor
  :: ( Hashable p
     , Ord (IsSU.SUHash u p)
     )
  => IsSU.SUHash u p -> BallotSUs u p -> SUBallot p
ballotFor suHash (BallotSUs ballotMap) =
  case Map.lookup suHash ballotMap of
    Nothing -> mempty
    Just ballotForSU -> ballotForSU

-- | Given a set of votes for an SU, compute the voting result.
addVotes
  :: (Hashable p)
  => StakeDistribution p
  -> Data.VotingResult
  -> SUBallot p
  -> Data.VotingResult
addVotes stakeDistribution votingResult (SUBallot ballot) =
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
updateBallot
  :: ( HasSigningScheme p
     , Hashable p
     , HasHash p (VKey p)
     )
  => BallotSUs u p -> IsVote.IsVote u p -> BallotSUs u p
updateBallot
  (BallotSUs ballot)
  isvote
  = BallotSUs
  $ Map.insertWith
      (\_newMap oldMap -> oldMap ⨃ [(hash (IsVote.voterSU isvote) , (IsVote.confidenceSU isvote))])
      (IsVote.votedSUHash isvote)
      mempty
      ballot
