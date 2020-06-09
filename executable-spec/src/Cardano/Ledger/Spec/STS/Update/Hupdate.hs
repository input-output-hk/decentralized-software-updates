{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Cardano.Ledger.Spec.STS.Update.Hupdate where

import           Data.Word (Word8)
import           GHC.Generics (Generic)

import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     initialRules, judgmentContext, trans, transitionRules,
                     wrapFailed)

import           Ledger.Core (BlockCount, Slot, SlotCount,  (*.))

import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.Classes.HasStakeDistribution
                     (HasStakeDistribution, StakePools (StakePools), SIPExperts (SIPExperts),
                     stakeDistribution)
import           Cardano.Ledger.Spec.Classes.TracksSlotTime (TracksSlotTime)
import qualified Cardano.Ledger.Spec.Classes.TracksSlotTime as SlotTime
import           Cardano.Ledger.Spec.State.ActivationState (ActivationState)
import qualified Cardano.Ledger.Spec.State.ActivationState as Activation
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import           Cardano.Ledger.Spec.STS.Update.Approval (APPROVAL)
import qualified Cardano.Ledger.Spec.STS.Update.Approval as Approval
import           Cardano.Ledger.Spec.STS.Update.TallyImplVotes (TIVOTES)
import qualified Cardano.Ledger.Spec.STS.Update.TallyImplVotes as TallyImplVotes
import           Cardano.Ledger.Spec.Classes.HasAdversarialStakeRatio (HasAdversarialStakeRatio, adversarialStakeRatio)
import qualified Cardano.Ledger.Update.Ideation as Ideation

-- | The Header Update STS
-- Incorporates "update logic" processing
-- at the block header level
data HUPDATE p

data Env p
 = Env { k :: !BlockCount
         -- ^ Chain stability parameter.
       , r_a :: !Float
         -- ^ adversary stake ratio
       , sipExpertsStakeDist :: !(StakeDistribution p)
       , prvNoQuorum :: !Word8
         -- ^ How many times a revoting is allowed due to a no quorum result
       , prvNoMajority :: !Word8
         -- ^ How many times a revoting is allowed due to a no majority result

       --------------------------------------------------------------------------------
       -- Additions needed to the environment by the approval phase.
       --------------------------------------------------------------------------------
       -- TODO: the fields below should be made strict.
       , slotsPerEpoch :: SlotCount
       , currentSlot   :: Slot
         -- ^ The current slot is currently part of the signal, but I think it
         -- is better to make it explicit in the environment that we're
         -- referring to the current slot. The rule that changes slots and epochs
         -- should maintain the state invariant that:
         --
         -- > epochFirstSlot <= currentSlot < epochFirstSlot + slotsPerEpoch
         --
         -- here state refers to the state associated to that rule.
         --
         -- So having the current slot in the environment makes it easier to
         -- ensure that the slot we pass to the rule is the rule that respects
         -- this invariant, since we will be transferring this from the state.
         --
         -- Then the chain tick rule (or header rule) would look something like:
         --
         -- > (... slotsPerEpoch ...)
         -- > |-
         -- > (... currentSlot, epochFirstSlot ...)
         -- > -- slot / SLOTTICK -->
         -- > (... currentSlot', epochFirstSlot' ...)
         -- >
         -- > Env { ... currentSlot', epochFirstSlot' }
         -- > |-
         -- > updateSt' -- HUPDATE --> updateSt'
         --
         -- The SLOTTICK rule could be something like:
         --
         -- > epochFirstSlot + slotsPerEpoch  == slot
         -- > ---------------------------------------
         -- > (currentSlot, epochFirstSlot)
         -- > -- slot / SLOTTICK -->
         -- > (slot, slot)
         --
         -- Plus the rules for the cases in which the first slot of the epoch
         -- doesn't change.
         --
         -- NOTE that it is __crucial__ that the chain tick rule
         -- __does not miss any slots__.
       , epochFirstSlot :: Slot
         -- ^ What is the slot of the first epoch.
       , stakepoolsDistribution :: (StakeDistribution p)
       }
       deriving (Show)

instance TracksSlotTime (Env p) where
  stableAfter    Env { k }              = 2 *. k
  currentSlot    Env { currentSlot }    = currentSlot
  slotsPerEpoch  Env {slotsPerEpoch }   = slotsPerEpoch
  epochFirstSlot Env { epochFirstSlot } = epochFirstSlot

instance HasStakeDistribution SIPExperts (Env p) p where
  stakeDistribution SIPExperts = sipExpertsStakeDist

instance HasStakeDistribution StakePools (Env p) p where
  stakeDistribution StakePools = stakepoolsDistribution

instance HasAdversarialStakeRatio (Env p) where
  adversarialStakeRatio = r_a


data St p
  = St { ideationSt   :: !(Ideation.State p)
       , approvalSt   :: !(State (APPROVAL p))
       , activationSt :: !(ActivationState p)
       }
       deriving (Show, Generic)

instance ( Hashable p
         ) =>  STS (HUPDATE p) where

  type Environment (HUPDATE p) = Env p

  type State (HUPDATE p) = St p

  type Signal (HUPDATE p) = Slot

  data PredicateFailure (HUPDATE p)
    = TIVOTESFailure    (PredicateFailure (TIVOTES p))
    deriving (Eq, Show)


  initialRules = [
    do
      IRC Env { } <- judgmentContext
      pure $! St { ideationSt   = Ideation.initialState
                 , approvalSt   = mempty
                 , activationSt = Activation.initialState undefined undefined
                 -- TODO: I can't conjure initial hashes and implementation data
                 -- here! Can't we just remove this initial rule?
                 }
    ]

  transitionRules = [
    do
      TRC ( env@Env { k
                    , r_a
                    , sipExpertsStakeDist
                    }
          , st@St { ideationSt
                  , approvalSt
                  , activationSt
                  }
          , slot
          ) <- judgmentContext

      --------------------------------------------------------------------------
      -- Ideation tick
      --------------------------------------------------------------------------
      let ideationSt' = Ideation.tick env ideationSt

      --------------------------------------------------------------------------
      -- Approval tick
      --------------------------------------------------------------------------
      approvalSt' <- trans @(TIVOTES p)
                           $ TRC ( TallyImplVotes.Env k sipExpertsStakeDist r_a
                                 , approvalSt
                                 , slot
                                 )

      --------------------------------------------------------------------------
      -- Activation tick
      --------------------------------------------------------------------------
      let activationSt' = Activation.tick env activationSt

      -- Depending on its type, each newly approved proposal has to:
      --
      -- - be considered for addition in the activation queue (which might
      --   additionaly result in the cancellation of the old proposal,
      --   cancellation of the new proposal, or changing the current candidate
      --   proposal by the new one).
      -- - be put in the list of approved application updates.
      -- - cancel a proposal in the activation queue.
      --
      let (proposalsState', activationSt'') =
            Activation.transferApprovals env
                                         (Approval.proposalsState approvalSt')
                                         activationSt'

      pure $! st { ideationSt   = ideationSt'
                 , approvalSt   = approvalSt' { Approval.proposalsState =  proposalsState'}
                 , activationSt = activationSt''
                 }
    ]

instance ( STS (TIVOTES p)
         , STS (HUPDATE p)
         ) => Embed (TIVOTES p) (HUPDATE p) where
  wrapFailed = TIVOTESFailure
