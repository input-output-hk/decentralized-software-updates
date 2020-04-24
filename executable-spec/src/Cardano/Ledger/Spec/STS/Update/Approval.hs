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

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, TRC (TRC), initialRules, judgmentContext,
                     transitionRules, (?!))
import           Ledger.Core (BlockCount, Slot,
                     dom,  (*.), (+.), (∈), (∉), (▷<=),
                     (⨃))

import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable
                     )
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     Signable, VKey, verify)
import           Cardano.Ledger.Spec.State.ApprovedSIPs (ApprovedSIPs,
                     isSIPApproved)
import           Cardano.Ledger.Spec.State.ProposalsState (revealProposal, isNotRevealed',
                     updateBallot, votingPeriodHasNotEnded, votingPeriodStarted)
import qualified Cardano.Ledger.Spec.State.ProposalsState as ProposalsState
import           Cardano.Ledger.Spec.State.WhenStable (WhenStable)
import           Cardano.Ledger.Spec.STS.Update.Approval.Data ( Submission (Submission),
                     ImplVote (ImplVote), Implementation, ImplementationData,
                     Payload (Reveal, Submit, Vote), SignedVoteData, commit,
                     confidence, implPayload, implSIPHash, sAuthor, sig,
                     vAuthor, vImplHash, vSig)
import           Cardano.Ledger.Spec.STS.Update.Approval.Data (IPSSt)
import           Cardano.Ledger.Spec.STS.Update.Data.Commit (Commit, calcCommit)
import           Cardano.Ledger.Spec.STS.Update.Ideation.Data (SIPHash)
import           Cardano.Ledger.Spec.State.ProposalState (VotingPeriod, Decision)
import           Cardano.Ledger.Spec.Classes.Indexed as Indexed
import           Cardano.Ledger.Spec.Classes.TracksSlotTime (TracksSlotTime, currentSlot)

data APPROVAL p

data Env p
  = Env
    { k              :: !BlockCount
    , mvps           :: !VotingPeriod
    -- ^ Maximum number of voting periods.
    , envCurrentSlot :: !Slot
    , apprvsips      :: !(ApprovedSIPs p)
    }
  deriving (Show, Generic)

data St p
  = St
    { wsimpls :: !(WhenStable (Commit p (Implementation p)))
    -- ^ Submitted commits, along with the timestamp (slot) at which they were
    -- submitted.
    , ipsst   :: !(IPSSt p)
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
    | NoStableAndCommittedImpl (Implementation p) (WhenStable (Commit p (Implementation p)))
    | NoApprovedSIP (SIPHash p) (ApprovedSIPs p)
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
      TRC ( Env { k, mvps, envCurrentSlot, apprvsips }
          , st@St { wsimpls, ipsst }
          , payload
          ) <- judgmentContext
      case payload of
        Submit (Submission { commit, sAuthor, sig }) -> do
          commit ∉ dom wsimpls ?! ImplAlreadySubmitted commit
          verify sAuthor commit sig ?! CommitSignatureDoesNotVerify
          pure $! st { wsimpls = wsimpls ⨃ [(commit, envCurrentSlot +. (2 *. k))]
                     }
        Reveal impl -> do
          -- Check that the corresponding commit is stable.
          calcCommit impl ∈ dom (wsimpls ▷<= envCurrentSlot)
            ?! NoStableAndCommittedImpl impl wsimpls
          -- Check that this proposal hasn't been revealed before.
          isNotRevealed' (implPayload impl) ipsst
            ?! ProposalAlreadyRevealed impl
          -- Check whether the referenced SIP in the metadata of the
          -- implementation metadata corresponds to an approved SIP.
          implSIPHash impl `isSIPApproved` apprvsips
            ?! NoApprovedSIP (implSIPHash impl) apprvsips
          pure $! st { ipsst = revealProposal
                                 envCurrentSlot
                                 mvps
                                 (implPayload impl)
                                 ipsst
                     }
        Vote (vote@ImplVote { vImplHash, confidence, vAuthor, vSig }) -> do
          votingPeriodStarted k envCurrentSlot vImplHash ipsst
            ?! VotePeriodHasNotStarted vImplHash envCurrentSlot ipsst
          votingPeriodHasNotEnded k envCurrentSlot vImplHash ipsst
            ?! VotePeriodHasEnded vImplHash envCurrentSlot ipsst
          verify vAuthor (vImplHash, confidence, vAuthor) vSig
            ?! VoteSignatureDoesNotVerify
          pure $! st { ipsst = updateBallot vImplHash vote ipsst }
    ]

deriving instance ( Hashable p
                  , HasSigningScheme p
                  ) => Eq (PredicateFailure (APPROVAL p))

deriving instance ( Hashable p
                  , HasSigningScheme p
                  ) => Show (PredicateFailure (APPROVAL p))

noApprovedSIP :: PredicateFailure (APPROVAL p) -> Maybe (SIPHash p)
noApprovedSIP (NoApprovedSIP sipHash _) = Just sipHash
noApprovedSIP _                         = Nothing

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
isSubmitted commit = isJust . Indexed.lookup commit . wsimpls

isStablySubmitted
  :: ( Hashable p
     , TracksSlotTime e
     )
  => e -> Commit p (Implementation p) -> St p -> Bool
isStablySubmitted e commit
  = maybe False (<= currentSlot e)
  . Indexed.lookup commit
  . wsimpls

isRevealed :: Hashable p => Hash p (ImplementationData p) -> St p -> Bool
isRevealed implHash = ProposalsState.isRevealed implHash . ipsst

isStablyRevealed
  :: ( Hashable p
     , TracksSlotTime e
     )
  => e -> Hash p (ImplementationData p) -> St p -> Bool
isStablyRevealed e implHash = ProposalsState.isStablyRevealed e implHash . ipsst

is
  :: Hashable p
  => Hash p (ImplementationData p) -> Decision -> St p -> Bool
is implHash d = ProposalsState.is implHash d . ipsst

isStably
  :: ( Hashable p
     , TracksSlotTime e
     )
  => e -> Hash p (ImplementationData p) -> Decision -> St p -> Bool
isStably e implHash d = ProposalsState.isStably e implHash d . ipsst
