{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Update where

import           GHC.Generics (Generic)
import           Data.Typeable (Typeable)

import qualified Test.QuickCheck as QC

import           Control.State.Transition.Trace (traceSignals, TraceOrder (OldestFirst))
import           Control.State.Transition (Embed, Environment, PredicateFailure, failBecause,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)
import qualified Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen
import           Data.AbstractSize (HasTypeReps)
import           Ledger.Core (Slot (Slot), BlockCount, (*.), SlotCount)

import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme)
import           Cardano.Ledger.Spec.State.ActiveSIPs (ActiveSIPs)
import           Cardano.Ledger.Spec.State.ApprovedSIPs (ApprovedSIPs)
import           Cardano.Ledger.Spec.State.WhenRevealedSIPs (WhenRevealedSIPs)
import           Cardano.Ledger.Spec.State.WhenSubmittedSIPs (WhenSubmittedSIPs)
import           Cardano.Ledger.Spec.State.Participants (Participants)
import           Cardano.Ledger.Spec.State.RevealedSIPs (RevealedSIPs)
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import           Cardano.Ledger.Spec.State.SubmittedSIPs (SubmittedSIPs)
import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import qualified Cardano.Ledger.Spec.STS.Update.Approval as Approval
import qualified Cardano.Ledger.Spec.STS.Update.Approval.Data as Approval.Data
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Cardano.Ledger.Spec.STS.Update.Ideation as Ideation
import qualified Cardano.Ledger.Spec.STS.Update.Ideation.Data as Ideation.Data
import qualified Cardano.Ledger.Spec.STS.Update.Implementation as Implementation
import           Cardano.Ledger.Spec.STS.Update.Approval (APPROVAL)
import           Cardano.Ledger.Spec.STS.Update.Ideation (IDEATION)
import           Cardano.Ledger.Spec.STS.Update.Implementation (IMPLEMENTATION)
import           Cardano.Ledger.Spec.State.ProposalState (VotingPeriod)
import qualified Cardano.Ledger.Spec.State.ActivationState as Activation
import           Cardano.Ledger.Spec.State.ActivationState (ActivationState)
import           Cardano.Ledger.Spec.Classes.TracksSlotTime (TracksSlotTime)
import qualified Cardano.Ledger.Spec.Classes.TracksSlotTime as SlotTime
import           Cardano.Ledger.Spec.STS.CanExtract (CanExtract, extractAll)


data UPDATE p

-- | As we incorporate more phases, like UP (or IMPLEMENTATION), we will be
-- adding more components to this environment.
--
-- See @Ideation.Env@ for more details on the meaning of each field.
data Env p
  = Env
    { k :: !BlockCount
    , maxVotingPeriods :: !VotingPeriod
    , currentSlot :: !Slot
    , asips :: !(ActiveSIPs p)
    , participants :: !(Participants p)
    , stakeDist :: !(StakeDistribution p)
    , apprvsips :: !(ApprovedSIPs p)
    , slotsPerEpoch :: !SlotCount
    , epochFirstSlot :: !Slot
    }
  deriving (Show, Generic)

instance TracksSlotTime (Env p) where
  stableAfter    Env { k }              = 2 *. k
  currentSlot    Env { currentSlot }    = currentSlot
  slotsPerEpoch  Env { slotsPerEpoch }  = slotsPerEpoch
  epochFirstSlot Env { epochFirstSlot } = epochFirstSlot

data St p
  = St
    { subsips :: !(SubmittedSIPs p)
    , wssips :: !(WhenSubmittedSIPs p)
    , wrsips :: !(WhenRevealedSIPs p)
    , sipdb :: !(RevealedSIPs p)
    , ballots :: !(Ideation.Data.SIPBallot p)
    , implementationSt :: State (IMPLEMENTATION p)
    , approvalSt :: !(State (APPROVAL p))
    -- TODO: once we get rid of the transitions above this one we can make this
    -- field strict.
    , activationSt :: ActivationState p
    }
  deriving (Show, Generic)

data UpdatePayload p
  = Ideation (Ideation.Data.IdeationPayload p)
  | Implementation Data.ImplementationPayload
  | Approval (Approval.Data.Payload p)
  | Activation (Activation.Endorsement p)
  deriving (Show, Generic)

deriving instance ( Typeable p
                  , HasTypeReps (Ideation.Data.IdeationPayload p)
                  , HasTypeReps (Approval.Data.Payload p)
                  , HasTypeReps (Activation.Endorsement p)
                  ) => HasTypeReps (UpdatePayload p)

instance ( Typeable p
         , HasTypeReps (Ideation.Data.IdeationPayload p)
         , HasTypeReps (Approval.Data.Payload p)
         , HasTypeReps (Activation.Endorsement p)
         ) => Sized (UpdatePayload p) where
  costsList _
    =  costsList (undefined :: Ideation.Data.IdeationPayload p)
    ++ costsList (undefined :: Data.ImplementationPayload)

instance ( Hashable p
         , HasSigningScheme p
         , STS (IDEATION p)
         , STS (APPROVAL p)
         ) => STS (UPDATE p) where

  type Environment (UPDATE p) = Env p

  type State (UPDATE p) = St p

  type Signal (UPDATE p) = UpdatePayload p

  data PredicateFailure (UPDATE p)
    = IdeationFailure (PredicateFailure (IDEATION p))
    | ImplementationFailure (PredicateFailure (IMPLEMENTATION p))
    | ApprovalFailure (PredicateFailure (APPROVAL p))
    | ActivationFailure (Activation.EndorsementError p)
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC ( env@Env { k
                    , maxVotingPeriods
                    , currentSlot
                    , asips
                    , participants
                    , stakeDist
                    , apprvsips
                    }
          , st@St { subsips
                  , wssips
                  , wrsips
                  , sipdb
                  , ballots
                  , implementationSt
                  , approvalSt
                  , activationSt
                  }
          , update
          ) <- judgmentContext

      case update of
        Ideation ideationPayload ->
          do
            Ideation.St { Ideation.subsips = subsips'
                        , Ideation.wssips = wssips'
                        , Ideation.wrsips = wrsips'
                        , Ideation.sipdb = sipdb'
                        , Ideation.ballots = ballots'
                        } <-
              trans @(IDEATION p)
                $ TRC ( Ideation.Env { Ideation.k = k
                                     , Ideation.currentSlot = currentSlot
                                     , Ideation.asips = asips
                                     , Ideation.participants = participants
                                     , Ideation.stakeDist = stakeDist
                                     }
                      , Ideation.St { Ideation.subsips = subsips
                                    , Ideation.wssips = wssips
                                    , Ideation.wrsips = wrsips
                                    , Ideation.sipdb = sipdb
                                    , Ideation.ballots = ballots
                                    }
                      , ideationPayload
                      )
            pure $ st { subsips = subsips'
                      , wssips = wssips'
                      , wrsips = wrsips'
                      , sipdb = sipdb'
                      , ballots = ballots'
                      }

        Implementation implementationPayload ->
          do
            implementationSt' <-
              trans @(IMPLEMENTATION p) $
              TRC ( Implementation.Env
                      currentSlot
                      apprvsips
                  , implementationSt
                  , implementationPayload
                  )
            pure $ st { implementationSt = implementationSt' }

        Approval approvalPayload ->
          do
            approvalSt' <-
              trans @(APPROVAL p) $ TRC ( Approval.Env k maxVotingPeriods currentSlot apprvsips
                                        , approvalSt
                                        , approvalPayload)
            pure st { approvalSt = approvalSt' }

        Activation endorsement ->
          either (\activationError -> do
                     failBecause (ActivationFailure activationError)
                     pure st
                 )
                 (\activationSt'   ->
                    pure $ st { activationSt = activationSt' }
                 )
                 (Activation.endorse endorsement env activationSt)
    ]

instance (STS (IDEATION p), STS (UPDATE p)) => Embed (IDEATION p) (UPDATE p) where
  wrapFailed = IdeationFailure

instance (STS (UPDATE p)) => Embed (IMPLEMENTATION p) (UPDATE p) where
  wrapFailed = ImplementationFailure

instance (STS (APPROVAL p), STS (UPDATE p)) => Embed (APPROVAL p) (UPDATE p) where
  wrapFailed = ApprovalFailure

data UPDATES p

instance ( Hashable p
         , HasSigningScheme p
         , STS (UPDATE p)
         ) => STS (UPDATES p) where

  type Environment (UPDATES p) = Environment (UPDATE p)

  type State (UPDATES p) = State (UPDATE p)

  type Signal (UPDATES p) = [Signal (UPDATE p)]

  data PredicateFailure (UPDATES p)
    = UpdateFailure (PredicateFailure (UPDATE p))
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC (env, st, updates) <- judgmentContext
      case updates of
        [] -> pure $! st
        (update:updates') ->
          do
            st' <- trans @(UPDATE p) $ TRC (env, st, update)
            trans @(UPDATES p) $ TRC (env, st', updates')
    ]


instance (STS (UPDATE p), STS (UPDATES p)) => Embed (UPDATE p) (UPDATES p) where
  wrapFailed = UpdateFailure

instance CanExtract (PredicateFailure (UPDATE p)) (PredicateFailure (APPROVAL p)) where
  extractAll (ApprovalFailure e) = [e]
  extractAll _                   = []

instance CanExtract (PredicateFailure (UPDATE p)) (Activation.EndorsementError p) where
  extractAll (ActivationFailure e) = [e]
  extractAll _                     = []

--------------------------------------------------------------------------------
-- Trace generators
--------------------------------------------------------------------------------

instance ( STS (UPDATES p)
         , STS.Gen.HasTrace (UPDATE p) a
         ) => STS.Gen.HasTrace (UPDATES p) a where

  envGen traceGenEnv = STS.Gen.envGen @(UPDATE p) traceGenEnv

  sigGen traceGenEnv env st
    =   traceSignals OldestFirst
    <$> STS.Gen.traceFrom @(UPDATE p) () 10 traceGenEnv env st
    -- We need to determine what is a realistic number of update
    -- transactions to be expected in a block.

  shrinkSignal =
    QC.shrinkList (STS.Gen.shrinkSignal @(UPDATE p) @a)

instance ( Hashable p
         , STS (UPDATE p)
         , STS.Gen.HasTrace (IDEATION p) ()
         ) => STS.Gen.HasTrace (UPDATE p) () where

  envGen traceGenEnv = do
    env <- STS.Gen.envGen @(IDEATION p) traceGenEnv
    pure $!
      Env { k = Ideation.k env
          , maxVotingPeriods = 0 -- TODO: define this if we move on with these generators.
          , currentSlot = Ideation.currentSlot env
          , asips = Ideation.asips env
          , participants = Ideation.participants env
          , stakeDist = Ideation.stakeDist env
          , apprvsips = mempty
          , slotsPerEpoch = 10 -- TODO: if needed we can make this arbitrary.
          , epochFirstSlot = Slot 0
          }

  sigGen
    ()
    Env { k, currentSlot, asips, participants, stakeDist }
    St { subsips, wssips, wrsips, sipdb, ballots }
    = do
    ideationPayload <-
      STS.Gen.sigGen
        @(IDEATION p)
        ()
        Ideation.Env { Ideation.k = k
                     , Ideation.currentSlot = currentSlot
                     , Ideation.asips = asips
                     , Ideation.participants = participants
                     , Ideation.stakeDist = stakeDist
                     }
        Ideation.St { Ideation.subsips = subsips
                    , Ideation.wssips = wssips
                    , Ideation.wrsips = wrsips
                    , Ideation.ballots = ballots
                    , Ideation.sipdb = sipdb
                    }
    pure $! Ideation ideationPayload

  shrinkSignal (Ideation ideationPayload) =
    Ideation <$> STS.Gen.shrinkSignal @(IDEATION p) @() ideationPayload
  shrinkSignal (Implementation {})        =
    error "Shrinking of IMPLEMENTATION signals is not defined yet."
  shrinkSignal (Approval {})              =
    error "Shrinking of APPROVAL signals is not defined yet."
  shrinkSignal (Activation {})            =
    error "Shrinking of activation signals is not defined yet."
