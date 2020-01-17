{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.Ballot where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Ledger.Core ((⨃))
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     VKey)
import           Cardano.Ledger.Spec.Classes.Indexed (Indexed, (!))
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)

-- | Register of votes for each candidate and voter.
newtype Ballot p c = Ballot (Map c (SIPBallot p))
  deriving stock (Eq, Show)
  deriving newtype (Core.Relation, Semigroup, Monoid, Indexed)

-- | Ballot associated to a given SIP.
newtype SIPBallot p = SIPBallot (Map (Hash p (VKey p)) Data.Confidence)

deriving instance Hashable p => Show (SIPBallot p)
deriving instance Hashable p => Eq (SIPBallot p)
deriving instance Hashable p => Core.Relation (SIPBallot p)
deriving instance Hashable p => Semigroup (SIPBallot p)
deriving instance Hashable p => Monoid (SIPBallot p)
deriving instance Hashable p => Indexed (SIPBallot p)

ballotFor
  :: ( Hashable p
     , Ord c
     )
  => c -> Ballot p c -> SIPBallot p
ballotFor aCandidate (Ballot ballotMap) =
  case Map.lookup aCandidate ballotMap of
    Nothing -> mempty
    Just ballotForSip -> ballotForSip

-- | Given a set of votes for an SIP, compute the voting result.
addVotes
  :: (Hashable p)
  => StakeDistribution p
  -> Data.VotingResult
  -> SIPBallot p
  -> Data.VotingResult
addVotes stakeDistribution votingResult (SIPBallot ballot) =
  Map.foldrWithKey'
  (\vkey someConfidence votingResult'
    -> Data.addVote (stake vkey) someConfidence votingResult'
  )
  votingResult
  ballot
  where
    stake vkey = fromMaybe err $ stakeDistribution ! vkey
      where
        err = error $ "No stake for key " ++ show vkey

-- | Update the ballot with the given vote.
--
-- Note that if a voter already cast a vote (according to the given ballot),
-- then the old vote is replaced with the new one.
updateBallot
  :: ( HasSigningScheme p
     , Hashable p
     , HasHash p (VKey p)
     , Vote v c p
     , Ord c
     )
  => Ballot p c -> v -> Ballot p c
updateBallot
  (Ballot ballot)
  vote
  = Ballot
  $ Map.insertWith
      (\_newMap oldMap -> oldMap ⨃ [(hash (voter vote), confidence vote)])
      (candidate vote)
      mempty
      ballot

class Vote v c p | v -> c p where
  candidate :: v -> c

  voter :: v -> VKey p

  confidence :: v -> Data.Confidence
