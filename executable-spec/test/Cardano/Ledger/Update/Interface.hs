{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Update.Interface

where

import           Control.Arrow (right)
import           Data.Bifunctor (bimap)
import           Data.Data (Data)

import           Control.State.Transition (PredicateFailure (PredicateFailure),
                     STS, TRC (TRC), applySTS)
import qualified Control.State.Transition as Transition


import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.State.ActiveSIPs (ActiveSIPs)
import           Cardano.Ledger.Spec.State.ApprovedSIPs (ApprovedSIPs)
import           Cardano.Ledger.Spec.State.Participants
                     (Participants (Participants))
import           Cardano.Ledger.Spec.State.ProposalState (VotingPeriod)
import           Cardano.Ledger.Spec.State.RevealedSIPs (RevealedSIPs)
import           Cardano.Ledger.Spec.State.SIPsVoteResults (SIPsVoteResults)
import qualified Cardano.Ledger.Spec.State.StakeDistribution as STS.StakeDistribution
import           Cardano.Ledger.Spec.State.SubmittedSIPs (SubmittedSIPs)
import           Cardano.Ledger.Spec.State.WhenRevealedSIPs (WhenRevealedSIPs)
import           Cardano.Ledger.Spec.State.WhenSubmittedSIPs (WhenSubmittedSIPs)
import           Cardano.Ledger.Spec.STS.Update (UpdatePayload)
import qualified Cardano.Ledger.Spec.STS.Update as Update
import           Cardano.Ledger.Spec.STS.Update.Approval (APPROVAL)
import qualified Cardano.Ledger.Spec.STS.Update.Hupdate as Hupdate
import           Cardano.Ledger.Spec.STS.Update.Ideation.Data (SIPBallot)
import qualified Cardano.Ledger.Spec.STS.Update.Implementation as Implementation
import           Ledger.Core (BlockCount)

-- | Interface state:
--
-- - it contains values needed by the environment and state of the SOS rules.
-- - it is modified either by the application of the SOS rules.
--
data IState p =
  IState
  { iStateK :: !BlockCount
  , iStateMaxVotingPeriods :: !VotingPeriod
  , iStateStakeDist :: !(STS.StakeDistribution.StakeDistribution p)
  , iStateCurrentSlot :: !Core.Slot
  , iStateSubsips :: !(SubmittedSIPs p)
  , iStateSipdb :: !(RevealedSIPs p)
  , iStateBallot :: !(SIPBallot p)
  , iStateR_a :: !Float
  , iStateWssips :: !(WhenSubmittedSIPs p)
  , iStateWrsips :: !(WhenRevealedSIPs p)
  , iStateAsips :: !(ActiveSIPs p)
  , iStatevresips :: !(SIPsVoteResults p)
  , iStateApprvsips :: !(ApprovedSIPs p)
  , iStateApproval :: !(Transition.State (APPROVAL p))
  } deriving (Show)

slotTick
  :: forall p
   . STS (Hupdate.HUPDATE p)
  => Core.Slot -> IState p -> Either (UIError p) (IState p)
slotTick slot iState
  = right tick
  $ bimap
      HupdateError
      (fromHupdateSt iState)
      $ applySTS @(Hupdate.HUPDATE p) (TRC (env, st, slot))
  where
    tick iState' = iState' { iStateCurrentSlot = slot }
    env = projectToHupdateEnv iState
    st = projecTotHupdateSt iState

applyUpdatePayload
  :: forall p
  .  STS (Update.UPDATE p)
  => UpdatePayload p -> IState p -> Either (UIError p) (IState p)
applyUpdatePayload updatePayload iState =
  bimap
    UpdateError
    (fromUpdateSt iState)
    $ applySTS @(Update.UPDATE p) (TRC (env, st, updatePayload))
  where
    env = projectToUpdateEnv iState
    st = projectToUpdateSt iState

-- | Sum type used to aggregate errors of the header and body update interfaces.
data UIError p
  = HupdateError [[PredicateFailure (Hupdate.HUPDATE p)]]
  | UpdateError [[PredicateFailure (Update.UPDATE p)]]
  deriving (Show)

deriving instance (Hashable p
                  , Data p
                  , Data (PredicateFailure (Hupdate.HUPDATE p))
                  , Data (PredicateFailure (Update.UPDATE p))
                  ) => Data (UIError p)

projectToHupdateEnv :: (IState p) -> Hupdate.Env p
projectToHupdateEnv
  IState { iStateK
         , iStateStakeDist
         , iStateSipdb
         , iStateBallot
         , iStateR_a
         }
  =
  Hupdate.Env
  { Hupdate.k = iStateK
  , Hupdate.sipdb = iStateSipdb
  , Hupdate.ballots = iStateBallot
  , Hupdate.r_a = iStateR_a
  , Hupdate.stakeDist = iStateStakeDist
   -- TODO: for now we're not allowing re-voting.
   --
  , Hupdate.prvNoQuorum = 0
  , Hupdate.prvNoMajority = 0
  }

projecTotHupdateSt :: IState p -> Hupdate.St p
projecTotHupdateSt
  IState
  { iStateWrsips
  , iStateAsips
  , iStatevresips
  , iStateApprvsips
  , iStateApproval
  }
  =
  Hupdate.St
  { Hupdate.wrsips = iStateWrsips
  , Hupdate.asips = iStateAsips
  , Hupdate.vresips = iStatevresips
  , Hupdate.apprvsips = iStateApprvsips
  , Hupdate.approvalSt = iStateApproval
  }

projectToUpdateEnv :: IState p -> Update.Env p
projectToUpdateEnv
  IState
  { iStateK
  , iStateMaxVotingPeriods
  , iStateCurrentSlot
  , iStateAsips
  , iStateStakeDist
  , iStateApprvsips
  }
  =
  Update.Env
  { Update.k = iStateK
  , Update.maxVotingPeriods = iStateMaxVotingPeriods
  , Update.currentSlot = iStateCurrentSlot
  , Update.asips = iStateAsips
  , Update.participants = Participants [] -- TODO: The participants should be removed from the STS state.
  , Update.stakeDist = iStateStakeDist
  , Update.apprvsips = iStateApprvsips
  }

projectToUpdateSt :: IState p -> Update.St p
projectToUpdateSt
  IState
  { iStateSubsips
  , iStateWssips
  , iStateWrsips
  , iStateSipdb
  , iStateBallot
  , iStateApproval
  }
  =
  Update.St
  { Update.subsips = iStateSubsips
  , Update.wssips = iStateWssips
  , Update.wrsips = iStateWrsips
  , Update.sipdb = iStateSipdb
  , Update.ballots = iStateBallot
  , Update.implementationSt = Implementation.St ()
  , Update.approvalSt = iStateApproval
  }

fromHupdateSt :: IState p -> Hupdate.St p -> IState p
fromHupdateSt iState hState =
  iState
  { iStateWrsips = Hupdate.wrsips hState
  , iStateAsips = Hupdate.asips hState
  , iStatevresips = Hupdate.vresips hState
  , iStateApprvsips = Hupdate.apprvsips hState
  , iStateApproval = Hupdate.approvalSt hState
  }

fromUpdateSt :: IState p -> Update.St p -> IState p
fromUpdateSt iState uState =
  iState
  { iStateSubsips = Update.subsips uState
  , iStateWssips = Update.wssips uState
  , iStateWrsips = Update.wrsips uState
  , iStateSipdb = Update.sipdb uState
  , iStateBallot = Update.ballots uState
  , iStateApproval = Update.approvalSt uState
  }
