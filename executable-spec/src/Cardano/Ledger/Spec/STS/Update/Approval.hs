{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE TypeApplications #-}


-- | SOS rules for the software improvement proposal rules.
module Cardano.Ledger.Spec.STS.Update.Approval where

import           GHC.Generics (Generic)
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           Data.Maybe (isJust)
import           Data.Map.Strict (Map)

import qualified Data.Map.Strict as Map

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, TRC (TRC), initialRules, judgmentContext,
                     transitionRules, (?!))
import           Ledger.Core (BlockCount, Slot, (*.))

import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable
                     )
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     Signable, VKey, verify)
import           Cardano.Ledger.Spec.State.ProposalsState (reveal, isNotRevealed',
                     updateBallot, votingPeriodHasNotEnded, votingPeriodHasStarted)
import qualified Cardano.Ledger.Spec.State.ProposalsState as ProposalsState
import           Cardano.Ledger.Spec.STS.Update.Approval.Data ( Submission (Submission),
                     ImplVote (ImplVote), Implementation, ImplementationData,
                     Payload (Reveal, Submit, Vote), SignedVoteData, commit,
                     confidence, implPayload, implSIPHash, sAuthor, sig,
                     vAuthor, vImplHash, vSig)
import           Cardano.Ledger.Spec.STS.Update.Approval.Data (IPSSt)
import           Cardano.Ledger.Spec.STS.Update.Data.Commit (Commit, calcCommit)
import           Cardano.Ledger.Spec.STS.Update.Ideation.Data (SIPHash)
import           Cardano.Ledger.Spec.State.ProposalState (VotingPeriod, Decision)
import           Cardano.Ledger.Spec.Classes.TracksSlotTime (TracksSlotTime, currentSlot, stableAt)
import qualified Cardano.Ledger.Spec.Classes.TracksSlotTime
import qualified Cardano.Ledger.Update.Ideation as Ideation
import           Cardano.Ledger.Spec.State.ProposalState (Decision(Approved))


data APPROVAL p


data Env p
  = Env
    { k              :: !BlockCount
    , mvps           :: !VotingPeriod
    -- ^ Maximum number of voting periods.
    , envCurrentSlot :: !Slot
    , ideationSt     :: !(Ideation.State p)
    }
  deriving (Generic)

-- | TODO: this instance and @Env@ should be removed in favor of a single
-- unified environment.
instance TracksSlotTime (Env p) where
  stableAfter env = 2 *. k env

  currentSlot env = envCurrentSlot env

  epochFirstSlot = undefined

  slotsPerEpoch = undefined

data St p
  = St
    { submissionStableAt :: !(Map (Commit p (Implementation p)) Core.Slot)
    -- ^ Submitted commits, along with the timestamp (slot) at which they were
    -- submitted.
    , proposalsState   :: !(IPSSt p)
    -- ^ Implementation-proposals state. See 'ProposalsState'.
    }
    deriving (Show, Generic)
    deriving Semigroup via GenericSemigroup (St p)
    deriving Monoid via GenericMonoid (St p)

instance ( Hashable p
         , HasSigningScheme p
         , HasHash p (VKey p) -- needed to bring the 'Ord' instance for 'SIP'.
         , HasHash p (Implementation p)
         , HasHash p (ImplementationData p)
         , HasHash p (Int, VKey p, Hash p (Implementation p))
         , Signable p (Commit p (Implementation p))
         , Signable p (SignedVoteData p)
         ) => STS (APPROVAL p) where

  type Environment (APPROVAL p) = Env p

  type State (APPROVAL p) = St p

  type Signal (APPROVAL p) = Payload p

  data PredicateFailure (APPROVAL p)
    = InvalidAuthor (VKey p)
    | ImplAlreadySubmitted (Commit p (Implementation p))
    | CommitSignatureDoesNotVerify
    | NoStableAndCommittedImpl (Implementation p) (Map (Commit p (Implementation p)) Core.Slot)
    | NoApprovedSIP (SIPHash p)
    | InvalidVoter (VKey p)
    | VoteSignatureDoesNotVerify
    | VotePeriodHasNotStarted
        (Hash p (ImplementationData p))
        Slot
        (IPSSt p)
    | VotePeriodHasEnded
        (Hash p (ImplementationData p))
        Slot
        (IPSSt p)
    | ProposalAlreadyRevealed (Implementation p)
    -- ^ No corresponding approved SIP (hash) was found among the approved SIP's.

  initialRules = []

  transitionRules = [
    do
      TRC ( env@Env { mvps, envCurrentSlot, ideationSt }
          , st@St { submissionStableAt, proposalsState }
          , payload
          ) <- judgmentContext
      case payload of
        Submit (Submission { commit, sAuthor, sig }) -> do
          not (isSubmitted commit st) ?! ImplAlreadySubmitted commit
          verify sAuthor commit sig ?! CommitSignatureDoesNotVerify
          pure $! st { submissionStableAt =
                       Map.insert commit (stableAt env (currentSlot env)) submissionStableAt
                     }
        Reveal impl -> do
          -- Check that the corresponding commit is stable.
          isStablySubmitted env (calcCommit impl) st
            ?! NoStableAndCommittedImpl impl submissionStableAt
          -- Check that this proposal hasn't been revealed before.
          isNotRevealed' (implPayload impl) proposalsState
            ?! ProposalAlreadyRevealed impl
          -- Check whether the referenced SIP in the metadata of the
          -- implementation metadata corresponds to an approved SIP.

          -- TODO: use the approved SIPs from the new ideation state. We might
          -- need to pass the ideation state to the APPROVAL environment
          Ideation.is (implSIPHash impl) Approved ideationSt
            ?! NoApprovedSIP (implSIPHash impl)

          pure $! st { proposalsState = reveal
                                 envCurrentSlot
                                 mvps
                                 (implPayload impl)
                                 proposalsState
                     }
        Vote (vote@ImplVote { vImplHash, confidence, vAuthor, vSig }) -> do
          verify vAuthor (vImplHash, confidence, vAuthor) vSig
            ?! VoteSignatureDoesNotVerify
          votingPeriodHasStarted env vImplHash proposalsState
            ?! VotePeriodHasNotStarted vImplHash envCurrentSlot proposalsState
          votingPeriodHasNotEnded env vImplHash proposalsState
            ?! VotePeriodHasEnded vImplHash envCurrentSlot proposalsState
          pure $! st { proposalsState = updateBallot vImplHash vote proposalsState }
    ]

deriving instance ( Hashable p
                  , HasSigningScheme p
                  ) => Eq (PredicateFailure (APPROVAL p))

deriving instance ( Hashable p
                  , HasSigningScheme p
                  ) => Show (PredicateFailure (APPROVAL p))

noApprovedSIP :: PredicateFailure (APPROVAL p) -> Maybe (SIPHash p)
noApprovedSIP (NoApprovedSIP sipHash ) = Just sipHash
noApprovedSIP _                        = Nothing

noStableAndCommitedImpl :: PredicateFailure (APPROVAL p) -> Maybe (Implementation p)
noStableAndCommitedImpl (NoStableAndCommittedImpl impl _) = Just impl
noStableAndCommitedImpl _                                 = Nothing

votePeriodHasNotStarted
  :: PredicateFailure (APPROVAL p) -> Maybe (Hash p (ImplementationData p))
votePeriodHasNotStarted (VotePeriodHasNotStarted implHash _ _) = Just implHash
votePeriodHasNotStarted _                                      = Nothing

votePeriodHasEnded :: PredicateFailure (APPROVAL p) -> Maybe (Hash p (ImplementationData p))
votePeriodHasEnded (VotePeriodHasEnded implHash _ _) = Just implHash
votePeriodHasEnded _                                 = Nothing

--------------------------------------------------------------------------------
-- Proposal state query operations
--------------------------------------------------------------------------------

isSubmitted
  :: Hashable p
  => Commit p (Implementation p) -> St p -> Bool
isSubmitted commit = isJust . Map.lookup commit . submissionStableAt

isStablySubmitted
  :: ( Hashable p
     , TracksSlotTime env
     )
  => env -> Commit p (Implementation p) -> St p -> Bool
isStablySubmitted env commit
  = maybe False (<= currentSlot env)
  . Map.lookup commit
  . submissionStableAt

isRevealed :: Hashable p => Hash p (ImplementationData p) -> St p -> Bool
isRevealed implHash = ProposalsState.isRevealed implHash . proposalsState

isStablyRevealed
  :: ( Hashable p
     , TracksSlotTime env
     )
  => env -> Hash p (ImplementationData p) -> St p -> Bool
isStablyRevealed env implHash =
  ProposalsState.isStablyRevealed env implHash . proposalsState

is
  :: Hashable p
  => Hash p (ImplementationData p) -> Decision -> St p -> Bool
is implHash d = ProposalsState.is implHash d . proposalsState

isStably
  :: ( Hashable p
     , TracksSlotTime env
     )
  => env -> Hash p (ImplementationData p) -> Decision -> St p -> Bool
isStably env implHash d = ProposalsState.isStably env implHash d . proposalsState
