{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Ledger.Spec.STS.Update.Ideation where

import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import qualified Data.Set as Set
import           GHC.Generics (Generic)

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, TRC (TRC), initialRules, judgmentContext,
                     transitionRules, (?!))
import           Ledger.Core (dom, (∈), (▷<=), (-.), (*.), (⨃), (⋪), (+.)
                             , Slot (Slot), BlockCount)

import           Cardano.Ledger.Spec.Classes.HasAuthor (author)
import           Cardano.Ledger.Spec.Classes.Hashable (Hashable, HasHash, Hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (VKey, HasSigningScheme, verify, Signable)
import           Cardano.Ledger.Spec.Classes.Indexed ((!), withValue)
import           Cardano.Ledger.Spec.State.ActiveSIPs (ActiveSIPs)
import           Cardano.Ledger.Spec.State.Ballot (updateBallot)
import           Cardano.Ledger.Spec.State.WhenRevealedSIPs (WhenRevealedSIPs)
import           Cardano.Ledger.Spec.State.WhenSubmittedSIPs (WhenSubmittedSIPs)
import           Cardano.Ledger.Spec.State.Participants (Participants)
import           Cardano.Ledger.Spec.State.RevealedSIPs (RevealedSIPs)
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import           Cardano.Ledger.Spec.State.SubmittedSIPs (SubmittedSIPs)
import           Cardano.Ledger.Spec.STS.Update.Ideation.Data
                     (IdeationPayload (Reveal, Submit, Vote), SIP,
                     SIPData)
import qualified Cardano.Ledger.Spec.STS.Update.Ideation.Data as Ideation.Data
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.STS.Update.Data.Commit (Commit, calcCommit)
import           Cardano.Ledger.Spec.STS.Update.Ideation.Data (SIPBallot)


--------------------------------------------------------------------------------
-- Updates ideation phase
--------------------------------------------------------------------------------

-- | Ideation phase of system updates
data IDEATION p

data Env p
  = Env
    { k :: !BlockCount
      -- ^ Chain stability parameter.
    , currentSlot :: !Slot
      -- ^ The current slot in the blockchain system
    , asips :: !(ActiveSIPs p)
    , participants :: !(Participants p)
    , stakeDist :: !(StakeDistribution p)
    }
  deriving (Show, Generic)

data St p
  = St
    { subsips :: !(SubmittedSIPs p)
    , wssips :: !(WhenSubmittedSIPs p)
    , wrsips :: !(WhenRevealedSIPs p)
    , sipdb :: !(RevealedSIPs p)
    , ballots :: !(SIPBallot p)
    }
  deriving (Show, Generic)
  deriving Semigroup via GenericSemigroup (St p)
  deriving Monoid via GenericMonoid (St p)

-- | Is the SIP stably revealed?
--
-- TODO: 'asips' should be put in the ideation state.
isStablyRevealed
  :: Hashable p
  => Env p
  -> Ideation.Data.SIPHash p
  -> Bool
isStablyRevealed Env { asips } sipHash = sipHash ∈ dom asips

isRevealed
  :: Hashable p
  => St p
  -> Ideation.Data.SIPHash p
  -> Bool
isRevealed St { wrsips } sipHash = sipHash ∈ dom wrsips

isStablySubmitted
  :: Hashable p
  => Env p
  -> St p
  -> Commit p (SIP p)
  -> Bool
isStablySubmitted Env { currentSlot, k } St { wssips } sipCommit
  =  Slot 0 +. 2 *. k <= currentSlot
  && sipCommit ∈ dom (wssips ▷<= (currentSlot -. (2 *. k)))
  -- To make the implementation of this function less awkward we could make
  -- wssips to record the time at which a submission becomes stable.

isSubmitted
  :: Hashable p
  => St p
  -> Commit p (SIP p)
  -> Bool
isSubmitted St { subsips } sipCommit =
  sipCommit ∈ dom subsips

instance ( Hashable p
         , HasHash p SIPData
         , HasHash p (SIP p)
         , HasHash p (Int, VKey p, Hash p (SIP p))
         , HasHash p (VKey p) -- needed to bring the 'Ord' instance for 'SIP'.
         , HasSigningScheme p
         , Signable p (Commit p (SIP p))
         , Signable p (Ideation.Data.SIPHash p, Data.Confidence, VKey p)
         ) => STS (IDEATION p) where

  type Environment (IDEATION p) = Env p

  type State (IDEATION p) = St p

  type Signal (IDEATION p) = IdeationPayload p

  -- | IDEATION phase failures
  data PredicateFailure (IDEATION p)
    = SIPAlreadySubmitted (Ideation.Data.SIP p)
    | NoSIPToReveal (Ideation.Data.SIP p)
    | SIPAlreadyRevealed (Ideation.Data.SIP p)
    | InvalidAuthor (VKey p)
    | NoStableAndCommittedSIP (Ideation.Data.SIP p) (WhenSubmittedSIPs p)
    | InvalidVoter (VKey p)
    | VoteNotForActiveSIP (Ideation.Data.SIPHash p)
    | VotingPeriodEnded (Ideation.Data.SIPHash p) Slot Slot
    | CommitSignatureDoesNotVerify
    | VoteSignatureDoesNotVerify
    deriving (Eq, Show)

  initialRules = [ pure $! mempty ]

  transitionRules = [
    do
      TRC ( env@Env { currentSlot
                    , asips
                    }
          , st@St { subsips
                  , wssips
                  , wrsips
                  , sipdb
                  , ballots
                  }
          , sig
          ) <- judgmentContext
      case sig of
        Submit sipc sip -> do
          not (isSubmitted st (Ideation.Data.commit sipc)) ?! SIPAlreadySubmitted sip

          verify (author sipc) (Ideation.Data.commit sipc) (Ideation.Data.upSig sipc) ?!
            CommitSignatureDoesNotVerify

          pure $! st { wssips = wssips ⨃ [(Ideation.Data.commit sipc, currentSlot)]
                     , subsips = subsips ⨃ [(Ideation.Data.commit sipc, sip)]
                     }

        Reveal sip -> do
          isSubmitted st (calcCommit sip) ?! NoSIPToReveal sip

          not (isRevealed st (Ideation.Data.sipHash sip)) ?! SIPAlreadyRevealed sip

          -- The Revealed SIP must correspond to a stable Commited SIP.
          -- Restrict the range of wssips to values less or equal than
          -- @currentSlot - 2k@
          --
          -- TODO: this won't work if the submission slot is < 2k. For instance
          -- if a SIP was submitted at slot 0 this condition ensures that it can
          -- also be revealed at slot 0.
          isStablySubmitted env st (calcCommit sip)
            ?! NoStableAndCommittedSIP sip wssips

          pure st { wssips = Set.singleton (calcCommit sip) ⋪ wssips
                  , wrsips = wrsips ⨃ [(Ideation.Data.sipHash sip, currentSlot)]
                  , sipdb = sipdb ⨃ [(Ideation.Data.sipHash sip, sip)]
                  }

        Vote voteForSIP -> do
            verify
              (Ideation.Data.voter voteForSIP)
              ( Ideation.Data.votedsipHash voteForSIP
              , Ideation.Data.confidence voteForSIP
              , Ideation.Data.voter voteForSIP
              )
              (Ideation.Data.voterSig voteForSIP)
              ?!
              VoteSignatureDoesNotVerify

            -- SIP must be an active SIP, i.e. it must belong to the set of
            -- stable revealed SIPs.
            isStablyRevealed env (Ideation.Data.votedsipHash voteForSIP) ?!
              VoteNotForActiveSIP (Ideation.Data.votedsipHash voteForSIP)

            -- The end of the voting period for this SIP must not have been
            -- reached yet.
            withValue (asips ! Ideation.Data.votedsipHash voteForSIP) () $
              \asipSlot ->
                currentSlot <= asipSlot ?!
                VotingPeriodEnded (Ideation.Data.votedsipHash voteForSIP) currentSlot asipSlot

            pure $ st { ballots = updateBallot ballots voteForSIP }
    ]
