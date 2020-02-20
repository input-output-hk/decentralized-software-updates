{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Update.TallyImplVotes where

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, TRC (TRC), initialRules, judgmentContext,
                     transitionRules)
import           Ledger.Core (BlockCount, Slot)

import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.State.ProposalsState (tally)
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution, totalStake)
import           Cardano.Ledger.Spec.STS.Update.Approval (APPROVAL, St (St),
                     ipsst)


-- | Tally implementation votes.
data TIVOTES p

data Env p
  = Env
    { k         :: !BlockCount
    , stakeDist :: !(StakeDistribution p)
    , r_a       :: !Float -- TODO: this should be a newtype and ensure it's value is between 0 and 1.
    }

instance Hashable p => STS (TIVOTES p) where

  type Environment (TIVOTES p) = Env p

  type State (TIVOTES p) = State (APPROVAL p)

  type Signal (TIVOTES p) = Slot

  data PredicateFailure (TIVOTES p) = NoFailure
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC ( Env { k, stakeDist, r_a }
          , st@St { ipsst }
          , currentSlot
          ) <- judgmentContext
      pure $! st { ipsst = tally k currentSlot (stakeDist, totalStake stakeDist) r_a ipsst }
    ]
