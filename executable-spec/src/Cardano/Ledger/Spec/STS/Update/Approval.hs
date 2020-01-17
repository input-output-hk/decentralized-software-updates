{-# LANGUAGE DeriveAnyClass #-}
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

-- | SOS rules for the software improvement proposal rules.
module Cardano.Ledger.Spec.STS.Update.Approval where

import           GHC.Generics (Generic)
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))

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
import           Cardano.Ledger.Spec.State.ProposalsState (revealProposal,
                     updateBallot, votingPeriodHasNotEnded, votingPeriodStarted)
import           Cardano.Ledger.Spec.State.WhenStable (WhenStable)
import           Cardano.Ledger.Spec.STS.Update.Approval.Data (
                     ImplVote (ImplVote), Implementation, ImplementationData,
                     Payload (Reveal, Submit, Vote), SignedVoteData, commit,
                     confidence, implPayload, implSIPHash, sAuthor, sig,
                     vAuthor, vImplHash, vSig)
import           Cardano.Ledger.Spec.STS.Update.Approval.Data (IPSSt)
import           Cardano.Ledger.Spec.STS.Update.Data.Commit (Commit, calcCommit)
import           Cardano.Ledger.Spec.STS.Update.Ideation.Data (SIPHash)
import           Cardano.Ledger.Spec.State.ProposalState (VotingPeriod)

data APPROVAL p

data Env p
  = Env
    { k           :: !BlockCount
    , mvps        :: !VotingPeriod
    -- ^ Maximum number of voting periods.
    , currentSlot :: !Slot
    , apprvsips   :: !(ApprovedSIPs p)
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
    -- ^ No corresponding approved SIP (hash) was found among the approved SIP's.

  initialRules = []

  transitionRules = [
    do
      TRC ( Env { k, mvps, currentSlot, apprvsips }
          , st@St { wsimpls, ipsst }
          , payload
          ) <- judgmentContext
      case payload of
        Submit { commit, sAuthor, sig } -> do
          commit ∉ dom wsimpls ?! ImplAlreadySubmitted commit
          verify sAuthor commit sig ?! CommitSignatureDoesNotVerify
          pure $! st { wsimpls = wsimpls ⨃ [(commit, currentSlot +. (2 *. k))]
                     }
        Reveal impl -> do
          -- Check that the corresponding commit is stable.
          calcCommit impl ∈ dom (wsimpls ▷<= currentSlot)
            ?! NoStableAndCommittedImpl impl wsimpls
          -- Check whether the referenced SIP in the metadata of the
          -- implementation metadata corresponds to an approved SIP.
          implSIPHash impl `isSIPApproved` apprvsips
            ?! NoApprovedSIP (implSIPHash impl) apprvsips
          pure $! st { ipsst = revealProposal
                                 currentSlot
                                 mvps
                                 (implPayload impl)
                                 ipsst
                     }
        Vote (vote@ImplVote { vImplHash, confidence, vAuthor, vSig }) -> do
          votingPeriodStarted k currentSlot vImplHash ipsst
            ?! VotePeriodHasNotStarted vImplHash currentSlot ipsst
          votingPeriodHasNotEnded k currentSlot vImplHash ipsst
            ?! VotePeriodHasEnded vImplHash currentSlot ipsst
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
