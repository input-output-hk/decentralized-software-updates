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

module Cardano.Ledger.Spec.STS.Update.Ideation where

import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import qualified Data.Set as Set
import           Data.Text as T
import           GHC.Exts (toList)
import           GHC.Generics (Generic)
import qualified Test.QuickCheck as QC

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, TRC (TRC), initialRules, judgmentContext,
                     transitionRules, (?!))
import           Ledger.Core (dom, (∈), (∉), (▷<=), (-.), (*.), (⨃), (⋪), range, (◁), Slot, BlockCount)

import qualified Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen

import qualified Cardano.Ledger.Generators.QuickCheck as Gen
import           Cardano.Ledger.Spec.Classes.Hashable (Hashable, hash, HasHash, Hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (sign, SKey, VKey, HasSigningScheme, verify, Signable)
import           Cardano.Ledger.Spec.Classes.Indexed ((!))
import           Cardano.Ledger.Spec.State.ActiveSIPs (ActiveSIPs)
import           Cardano.Ledger.Spec.State.Ballot (Ballot, updateBallot)
import           Cardano.Ledger.Spec.State.WhenRevealedSIPs (WhenRevealedSIPs)
import           Cardano.Ledger.Spec.State.WhenSubmittedSIPs (WhenSubmittedSIPs)
import           Cardano.Ledger.Spec.State.Participants (Participants)
import           Cardano.Ledger.Spec.State.RevealedSIPs (RevealedSIPs)
import           Cardano.Ledger.Spec.State.SubmittedSIPs (SubmittedSIPs)
import           Cardano.Ledger.Spec.STS.Update.Data
                     (IdeationPayload (Reveal, Submit, Vote), SIP (SIP),
                     SIPData (SIPData))
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data

import Cardano.Ledger.Test.Mock (Mock)

--------------------------------------------------------------------------------
-- Updates ideation phase
--------------------------------------------------------------------------------

-- | Ideation phase of system updates
data IDEATION p

-- Environmnet of the Ideation phase
data Env p
  = Env
    { k :: !BlockCount
      -- ^ Chain stability parameter.
    , currentSlot :: !Slot
      -- ^ The current slot in the blockchain system
    , asips :: !(ActiveSIPs p)
    , participants :: !(Participants p)
    }
  deriving (Show, Generic)

-- | Ideation phase state
--
data St p
  = St
    { subsips :: !(SubmittedSIPs p)
    , wssips :: !(WhenSubmittedSIPs p)
    , wrsips :: !(WhenRevealedSIPs p)
    , sipdb :: !(RevealedSIPs p)
    , ballots :: !(Ballot p)
    }
  deriving (Show, Generic)
  deriving Semigroup via GenericSemigroup (St p)
  deriving Monoid via GenericMonoid (St p)

instance ( Hashable p
         , HasHash p SIPData
         , HasHash p (SIP p)
         , HasHash p (Int, VKey p, Hash p (SIP p))
         , HasHash p (VKey p) -- needed to bring the 'Ord' instance for 'SIP'.
         , HasSigningScheme p
         , Signable p (Data.Commit p)
         , Signable p (Data.SIPHash p, Data.Confidence, VKey p)
         ) => STS (IDEATION p) where

  type Environment (IDEATION p) = Env p

  type State (IDEATION p) = St p

  type Signal (IDEATION p) = IdeationPayload p

  -- | IDEATION phase failures
  data PredicateFailure (IDEATION p)
    = SIPAlreadySubmitted (Data.SIP p)
   -- | SIPSubmittedAlreadyRevealed (Data.SIP p)
    | NoSIPToReveal (Data.SIP p)
    | SIPAlreadyRevealed (Data.SIP p)
    | InvalidAuthor (VKey p)
    | NoStableAndCommittedSIP (Data.SIP p) (WhenSubmittedSIPs p)
    | InvalidVoter (VKey p)
    | VoteNotForActiveSIP (Data.SIPHash p)
    | VotingPeriodEnded (Data.SIPHash p) Slot
    | CommitSignatureDoesNotVerify
    | VoteSignatureDoesNotVerify
    deriving (Eq, Show)

  initialRules = [ pure $! mempty ]

  transitionRules = [
    do
      TRC ( Env { k
                , currentSlot
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
          -- TODO: we should use the stake distribution instead: the
          -- participants is a generator only variable, and should probably be
          -- part of the generator environment.
          -- hash (Data._author sipc) ∈ dom stakeDist ?! InvalidAuthor (Data.author sip)
          Data.commit sipc ∉ dom subsips ?! SIPAlreadySubmitted sip

          verify (Data._author sipc) (Data.commit sipc) (Data.upSig sipc) ?!
            CommitSignatureDoesNotVerify

          pure $! st { wssips = wssips ⨃ [(Data.commit sipc, currentSlot)]
                     , subsips = subsips ⨃ [(Data.commit sipc, sip)]
                     }

        Reveal sip -> do
          -- TODO: use the stake distribution.
          -- hash (Data.author sip) ∈ dom stakeDist ?! InvalidAuthor (Data.author sip)
          (Data.calcCommit sip) ∈ dom subsips ?! NoSIPToReveal sip

          sip ∉ range sipdb ?! SIPAlreadyRevealed sip

          -- The Revealed SIP must correspond to a stable Commited SIP.
          -- Restrict the range of wssips to values less or equal than
          -- @currentSlot - 2k@
          Data.calcCommit sip
            ∈ dom (wssips ▷<= (currentSlot -. (2 *. k)))
            ?! NoStableAndCommittedSIP sip wssips

          pure st { wssips = Set.singleton (Data.calcCommit sip) ⋪ wssips
                  , wrsips = wrsips ⨃ [(Data.sipHash sip, currentSlot)]
                  , sipdb = sipdb ⨃ [(Data.sipHash sip, sip)]
                  }

        Vote voteForSIP -> do
            -- voter must be a stakeholder
            -- TODO: use the stake distribution.
            -- hash (Data.voter voteForSIP) ∈ dom stakeDist ?!
            --   InvalidVoter (Data.voter voteForSIP)

            verify
              (Data.voter voteForSIP)
              ( Data.votedsipHash voteForSIP
              , Data.confidence voteForSIP
              , Data.voter voteForSIP
              )
              (Data.voterSig voteForSIP)
              ?!
              VoteSignatureDoesNotVerify

            -- SIP must be an active SIP, i.e. it must belong to the set of
            -- stable revealed SIPs
            Data.votedsipHash voteForSIP ∈ dom asips ?!
              VoteNotForActiveSIP (Data.votedsipHash voteForSIP)

            -- The end of the voting period for this SIP must not have been
            -- reached yet.
            currentSlot <= asips ! Data.votedsipHash voteForSIP ?!
              VotingPeriodEnded (Data.votedsipHash voteForSIP) currentSlot

            pure $ st { ballots = updateBallot ballots voteForSIP }
    ]

instance STS.Gen.HasTrace (IDEATION Mock) a where

  envGen :: a -> QC.Gen (Env Mock)
  envGen _traceGenEnv
    = do
    someK <- Gen.k
    someCurrentSlot <- Gen.currentSlot
    someParticipants <- Gen.participants
    let env = Env { k = someK
                  , currentSlot = someCurrentSlot
                  , asips = mempty
                  , participants = someParticipants
                  }
    pure env

  sigGen
    _traceGenEnv
    Env{ k, currentSlot, participants }
    St{ wssips, wrsips, subsips } =
      case Set.toList $ range $ stableCommits ◁ subsips of
        [] ->
          -- There are no stable commits, so we can only generate a submission.
          submissionGen
        xs ->
          -- We use a 50/50 submission to revelation ratio. This can be changed
          -- if necessary.
          QC.frequency [ (1, submissionGen)
                       , (1, revelationGen xs)
                       ]
    where
      stableCommits = dom (wssips ▷<= (currentSlot -. (2 *. k)))
      submissionGen = do
        -- WARNING: suchThat can be very inefficient if this condition fails often.
        (sip, skeyAuthor) <- sipGen `QC.suchThat` ((`Set.notMember` range subsips) . fst)
        pure $! mkSubmission skeyAuthor sip
          where
            sipGen = do
              (vkeyAuthor, skeyAuthor) <- QC.elements $ toList participants
              sipMData <- sipMetadataGen
              sipData <- sipDataGen sipMData
              let sipHash = Data.SIPHash $ hash sipData
              salt <- Gen.bounded 2
              pure $! (SIP sipHash vkeyAuthor salt sipData, skeyAuthor)
                where
                  sipMetadataGen
                    = Data.SIPMetadata
                    <$> versionFromGen
                    <*> versionToGen
                    <*> QC.elements [Data.Impact, Data.NoImpact]
                    <*> QC.sublistOf [ Data.BlockSizeMax
                                     , Data.TxSizeMax
                                     , Data.SlotSize
                                     , Data.EpochSize
                                     ]
                    <*> QC.elements [Data.VPMin, Data.VPMedium, Data.VPLarge]
                    where
                      versionFromGen = do
                        protVer <- word64Gen
                        apVer <- word64Gen
                        pure $! (Data.ProtVer protVer, Data.ApVer apVer)

                      versionToGen = versionFromGen

                      word64Gen = Gen.bounded 5

                  sipDataGen sipMData = SIPData <$> (Data.URL <$> urlText) <*> (pure sipMData)
                    where
                      urlText = do
                        n <- QC.choose (0, 20)
                        str <- QC.vectorOf n QC.arbitraryUnicodeChar
                        pure $! T.pack str

            mkSubmission :: SKey Mock -> SIP Mock -> IdeationPayload Mock
            mkSubmission skey sip = Submit sipCommit sip
              where
                sipCommit =
                  Data.SIPCommit commit (Data.author sip) sipCommitSignature
                  where
                    commit = Data.calcCommit sip
                    sipCommitSignature = sign commit skey

      revelationGen subsipsList = do
        -- Problem! 'QC.suchThat' will loop forever if it cannot find a
        -- revelation that satisfies the given predicate!
        sip <- QC.elements subsipsList
        if Data.sipHash sip ∉ dom wrsips
          then pure $! Reveal sip
          else submissionGen


  -- It doesn't seem plausible that the @IdeationPayload@ can be shrunk in a
  -- meaningful way. If we shrink the SIP, we have to modify the commit, but
  -- then all the ballots will refer to invalid commits.
  --
  -- With this trace generation framework it might not be possible to get good
  -- shrinks for these signals.
  shrinkSignal (Submit _sipc _sip) = []
  shrinkSignal (Reveal _sip) = []
  shrinkSignal (Vote _ballot) = []
