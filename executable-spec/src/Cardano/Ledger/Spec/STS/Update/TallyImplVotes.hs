{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Update.TallyImplVotes where

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, TRC (TRC), initialRules, judgmentContext,
                     transitionRules)
import           Ledger.Core (BlockCount, Slot, (*.))

import           Cardano.Ledger.Spec.Classes.HasAdversarialStakeRatio
                     (HasAdversarialStakeRatio, adversarialStakeRatio)
import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.Classes.HasStakeDistribution
                     (HasStakeDistribution,
                     TechnicalExperts (TechnicalExperts))
import qualified Cardano.Ledger.Spec.Classes.HasStakeDistribution
import           Cardano.Ledger.Spec.Classes.TracksSlotTime (TracksSlotTime)
import qualified Cardano.Ledger.Spec.Classes.TracksSlotTime
import           Cardano.Ledger.Spec.State.ProposalsState (tally)
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import           Cardano.Ledger.Spec.STS.Update.Approval (APPROVAL,
                     proposalsState)


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
      TRC ( env
          , st
          , currentSlot
          ) <- judgmentContext
      pure $! st { proposalsState = tally (env, currentSlot) TechnicalExperts (proposalsState st) }
    ]

--------------------------------------------------------------------------------
-- TODO: these instances and @Env@ should be removed in favor of a single
-- unified environment.
--------------------------------------------------------------------------------

instance TracksSlotTime ((Env p, Slot)) where
  stableAfter (env, _) = 2 *. k env

  currentSlot (_, slot) = slot

  epochFirstSlot = undefined

  slotsPerEpoch = undefined

instance HasStakeDistribution TechnicalExperts ((Env p), Slot) p where
  stakeDistribution TechnicalExperts (env, _) = stakeDist env

instance HasAdversarialStakeRatio ((Env p, Slot)) where
  adversarialStakeRatio (env, _) = r_a env
