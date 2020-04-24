{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Update.Interface

where

import           Data.Bifunctor (bimap)
import           Data.Data (Data)
import           GHC.Generics (Generic)

import           Control.State.Transition (PredicateFailure (PredicateFailure),
                     STS, TRC (TRC), applySTS)
import qualified Control.State.Transition as Transition


import           Ledger.Core (BlockCount, (*.), (+.))
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.Classes.TracksSlotTime (TracksSlotTime,
                     currentSlot, epochFirstSlot, nextEpochFirstSlot,
                     stableAfter)
import qualified Cardano.Ledger.Spec.Classes.TracksSlotTime as SlotTime
import           Cardano.Ledger.Spec.State.ActivationState (ActivationState)
import qualified Cardano.Ledger.Spec.State.ActivationState as ActivationState
import           Cardano.Ledger.Spec.State.ActiveSIPs (ActiveSIPs)
import           Cardano.Ledger.Spec.State.ApprovedSIPs (ApprovedSIPs)
import           Cardano.Ledger.Spec.State.Participants
                     (Participants (Participants))
import           Cardano.Ledger.Spec.State.ProposalState (VotingPeriod)
import           Cardano.Ledger.Spec.State.ProposalState (Decision)
import           Cardano.Ledger.Spec.State.RevealedSIPs (RevealedSIPs)
import           Cardano.Ledger.Spec.State.SIPsVoteResults (SIPsVoteResults)
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import qualified Cardano.Ledger.Spec.State.StakeDistribution as STS.StakeDistribution
import           Cardano.Ledger.Spec.State.SubmittedSIPs (SubmittedSIPs)
import           Cardano.Ledger.Spec.State.WhenRevealedSIPs (WhenRevealedSIPs)
import           Cardano.Ledger.Spec.State.WhenSubmittedSIPs (WhenSubmittedSIPs)
import           Cardano.Ledger.Spec.STS.CanExtract (CanExtract, extractAll)
import           Cardano.Ledger.Spec.STS.Update (UpdatePayload)
import qualified Cardano.Ledger.Spec.STS.Update as Update
import           Cardano.Ledger.Spec.STS.Update.Approval (APPROVAL)
import           Cardano.Ledger.Spec.STS.Update.Approval.Data
                     (ImplementationAndHash, ProtocolVersion)
import qualified Cardano.Ledger.Spec.STS.Update.Approval.Data as Approval.Data
import qualified Cardano.Ledger.Spec.STS.Update.Hupdate as Hupdate
import qualified Cardano.Ledger.Spec.STS.Update.Ideation as Ideation
import           Cardano.Ledger.Spec.STS.Update.Ideation.Data (SIPBallot)
import qualified Cardano.Ledger.Spec.STS.Update.Implementation as Implementation

import           Cardano.Ledger.Assert

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
  , iStateEpochFirstSlot :: !Core.Slot
  , iStateSlotsPerEpoch :: !Core.SlotCount
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
  , iStateActivation :: !(ActivationState p)
  , iStateStakepoolsDistribution :: !(StakeDistribution p)
  } deriving (Show)

iStateProtocolVersion :: Hashable p => IState p -> ProtocolVersion
iStateProtocolVersion = Approval.Data.version . iStateCurrentVersion

iStateCurrentVersion :: IState p -> ImplementationAndHash p
iStateCurrentVersion = ActivationState.currentVersion . iStateActivation

-- | At which slot is will the current slot (according to the given state)
-- become stable.
currentSlotStableAt :: IState p -> Core.Slot
currentSlotStableAt st = stableAt (iStateCurrentSlot st) st

-- | At which slot will the given slot become stable.
--
stableAt :: Core.Slot -> IState p -> Core.Slot
stableAt slot st = slot +. stableAfter st

-- | Is the given slot stable?
isStable :: Core.Slot -> IState p -> Bool
isStable slot st = stableAt slot st <= iStateCurrentSlot st

instance TracksSlotTime (IState p) where
  currentSlot = iStateCurrentSlot

  stableAfter = (2 *.) . iStateK

  slotsPerEpoch = iStateSlotsPerEpoch

  epochFirstSlot = iStateEpochFirstSlot

slotTick
  :: forall p
   . STS (Hupdate.HUPDATE p)
  => Core.Slot -> IState p -> Either (UIError p) (IState p)
slotTick slot iState
  -- TODO: with the addition of a @iStateEpochFirstSlot@ and
  -- @iStateSlotsPerEpoch@ we have to ensure we don't go back in time.
  --
  -- So the Hupdate rule (or probably a rule above it) should make sure we don't
  -- go back in time, and the invariants are satisfied.
  = assert preconditionsHold
  $ tickTill slot iState
  where
    preconditionsHold = do
      currentSlot iState <=! slot


tickTill
  :: forall p
   . STS (Hupdate.HUPDATE p)
  => Core.Slot -> IState p -> Either (UIError p) (IState p)
tickTill desiredSlot iState
  | iStateCurrentSlot iState == desiredSlot =
      pure iState
  | iStateCurrentSlot iState <  desiredSlot = do
     st' <- bimap
              HupdateError
              (fromHupdateSt iState')
               $ applySTS @(Hupdate.HUPDATE p) (TRC (env, st, nextSlot))
     tickTill desiredSlot st'
  | otherwise                               =
    error "desired slot should be less or equal than the current slot"
  where
    nextSlot = iStateCurrentSlot iState +. 1
    iState'  = SlotTime.checkInvariants
             $ iState { iStateCurrentSlot    = nextSlot
                      , iStateEpochFirstSlot =
                          if nextEpochFirstSlot iState <= nextSlot
                          then nextEpochFirstSlot iState
                          else epochFirstSlot iState
                      }
    env = projectToHupdateEnv iState'
    st  = projecTotHupdateSt iState'


-- TODO: the payload parameter might be named 'payload' by the client, so we
-- might as well omit 'Payload' from the function name.
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

getUpdateErrors :: UIError p -> [[PredicateFailure (Update.UPDATE p)]]
getUpdateErrors (HupdateError _)  = []
getUpdateErrors (UpdateError ess) = ess

deriving instance (Hashable p
                  , Data p
                  , Data (PredicateFailure (Hupdate.HUPDATE p))
                  , Data (PredicateFailure (Update.UPDATE p))
                  ) => Data (UIError p)

instance CanExtract (UIError p) (PredicateFailure (APPROVAL p)) where
  extractAll (UpdateError ess) = concatMap (concatMap extractAll) ess
  extractAll _                 = []

instance CanExtract (UIError p) (ActivationState.EndorsementError p) where
  extractAll (UpdateError ess) = concatMap (concatMap extractAll) ess
  extractAll _                 = []

projectToHupdateEnv :: (IState p) -> Hupdate.Env p
projectToHupdateEnv
  IState { iStateK
         , iStateStakeDist
         , iStateSipdb
         , iStateBallot
         , iStateR_a
         , iStateCurrentSlot
         , iStateSlotsPerEpoch
         , iStateEpochFirstSlot
         , iStateStakepoolsDistribution
         }
  =
  Hupdate.Env
  { Hupdate.k = iStateK
  , Hupdate.sipdb = iStateSipdb
  , Hupdate.ballots = iStateBallot
  , Hupdate.r_a = iStateR_a
  , Hupdate.stakeDist = iStateStakeDist
  , Hupdate.currentSlot = iStateCurrentSlot
  , Hupdate.slotsPerEpoch = iStateSlotsPerEpoch
  , Hupdate.epochFirstSlot = iStateEpochFirstSlot
  , Hupdate.stakepoolsDistribution = iStateStakepoolsDistribution
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
  , iStateActivation
  }
  =
  Hupdate.St
  { Hupdate.wrsips = iStateWrsips
  , Hupdate.asips = iStateAsips
  , Hupdate.vresips = iStatevresips
  , Hupdate.apprvsips = iStateApprvsips
  , Hupdate.approvalSt = iStateApproval
  , Hupdate.activationSt = iStateActivation
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
  , iStateSlotsPerEpoch
  , iStateEpochFirstSlot
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
  , Update.slotsPerEpoch = iStateSlotsPerEpoch
  , Update.epochFirstSlot = iStateEpochFirstSlot
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
  , iStateActivation
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
  , Update.activationSt = iStateActivation
  }

projectToIdeationSt :: IState p -> Ideation.St p
projectToIdeationSt
  IState
  { iStateSubsips
  , iStateWssips
  , iStateWrsips
  , iStateSipdb
  , iStateBallot
  }
  =
  Ideation.St
    { Ideation.subsips = iStateSubsips
    , Ideation.wssips  = iStateWssips
    , Ideation.wrsips  = iStateWrsips
    , Ideation.sipdb   = iStateSipdb
    , Ideation.ballots = iStateBallot
    }

--  TODO: we should remove the need for this projection. We don't want to copy
--  state back and forth unnecessarily.
projectToIdeationEnv :: IState p -> Ideation.Env p
projectToIdeationEnv
  IState
  { iStateK
  , iStateCurrentSlot
  , iStateAsips
  , iStateStakeDist
  }
  =
  Ideation.Env
  { Ideation.k            = iStateK
  , Ideation.currentSlot  = iStateCurrentSlot
  , Ideation.asips        = iStateAsips
  , Ideation.participants = Participants [] -- TODO: The participants should be removed from the STS state.
  , Ideation.stakeDist    = iStateStakeDist
  }

fromHupdateSt :: IState p -> Hupdate.St p -> IState p
fromHupdateSt iState hState =
  iState
  { iStateWrsips = Hupdate.wrsips hState
  , iStateAsips = Hupdate.asips hState
  , iStatevresips = Hupdate.vresips hState
  , iStateApprvsips = Hupdate.apprvsips hState
  , iStateApproval = Hupdate.approvalSt hState
  , iStateActivation = Hupdate.activationSt hState
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
  , iStateActivation = Update.activationSt uState
  }

--------------------------------------------------------------------------------
-- Update state
--------------------------------------------------------------------------------

data UpdateState
  = Unknown
  -- ^ The update is not know to the update-state
  | SIP PhaseState
  | Implementation PhaseState
  | Queued
  | ActivationExpired
  | ActivationCanceled
  | ActivationUnsupported
  | BeingEndorsed
  | HasEnoughEndorsements
  | Scheduled
  | Activated
  | Canceled
  deriving (Eq, Show, Generic)



data PhaseState
  = Submitted
  | StablySubmitted
  | Revealed
  | StablyRevealed
  | Is Decision
  | IsStably Decision
  deriving (Eq, Show, Generic)
