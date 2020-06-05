
module Cardano.Ledger.Spec.Classes.HasVotingPeriodsCap where

import           Cardano.Ledger.Spec.State.ProposalState (VotingPeriod)


-- | Environments that specify a maximum number of voting periods.
class HasVotingPeriodsCap env where
  maxVotingPeriods :: env -> VotingPeriod
