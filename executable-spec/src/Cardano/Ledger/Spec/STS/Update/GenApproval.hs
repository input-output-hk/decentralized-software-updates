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

module Cardano.Ledger.Spec.STS.Update.GenApproval where

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
import           Ledger.Core (dom, (∈), (∉), (▷<=), (-.), (*.), (⨃), (⋪), range, (◁)
                             , Slot, SlotCount (SlotCount), BlockCount, (▷>=))

import qualified Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen

import qualified Cardano.Ledger.Generators.QuickCheck as Gen
import           Cardano.Ledger.Spec.Classes.Hashable (Hashable, hash, HasHash, Hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (sign, SKey, VKey, HasSigningScheme, verify, Signable)
import           Cardano.Ledger.Spec.Classes.Indexed ((!))
import           Cardano.Ledger.Spec.State.ActiveSUs (ActiveSUs)
import           Cardano.Ledger.Spec.State.Ballot (Ballot, updateBallot)
import           Cardano.Ledger.Spec.State.WhenRevealedSUs (WhenRevealedSUs)
import           Cardano.Ledger.Spec.State.WhenSubmittedSUs (WhenSubmittedSUs)
import           Cardano.Ledger.Spec.State.Participants (Participants)
import           Cardano.Ledger.Spec.State.RevealedSUs (RevealedSUs)
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import           Cardano.Ledger.Spec.State.SubmittedSUs (SubmittedSUs)
import           Cardano.Ledger.Spec.STS.Update.Data
                     (SUPayload (RevealSU, SubmitSU, VoteSU), SIP (SIP),
                     SIPData (SIPData))
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data

import Cardano.Ledger.Test.Mock (Mock)

--------------------------------------------------------------------------------
-- This a generic approval STS to be utilized by both
-- the Ideation and Approval STSs.
--------------------------------------------------------------------------------

-- | A Generic Approval STS.
-- Implements a generic apporval STS to be instantiated by the
-- `Ideation` STS and the `Approval` STS.
-- It is polymorphic in the hashing and signing algorithm (1st parameter) and in
-- the type of the software update (`SIP` or `UP`) (2nd parameter)
-- the contents of the software update (3rd parameter)
data GENAPPROVAL p u d

-- Environmnet of the ApprovalGen phase
data Env p d
  = Env
    { k :: !BlockCount
      -- ^ Chain stability parameter.
    , currentSlot :: !Slot
      -- ^ The current slot in the blockchain system
    , aSUs :: !(ActiveSUs p d)
    , participants :: !(Participants p)
    , stakeDist :: !(StakeDistribution p)
    }
  deriving (Show, Generic)

-- | Generic Approval state
--
data St p u d
  = St
    { subSUs :: !(SubmittedSUs p u)
    , wsSUs :: !(WhenSubmittedSUs p u)
    , wrSUs :: !(WhenRevealedSUs p d)
    , sudb :: !(RevealedSUs p d)
    , ballots :: !(Ballot p)
    }
  deriving (Show, Generic)
  deriving Semigroup via GenericSemigroup (St p)
  deriving Monoid via GenericMonoid (St p)

instance ( Hashable p
         , HasHash p Data.SIPData
         , HasHash p d
         , HasHash p (Data.SU p d)
         , HasHash p (Int, VKey p, Hash p (Data.SU p d))
         , HasHash p (VKey p) -- needed to bring the 'Ord' instance for 'SU'.
         , HasSigningScheme p
         , Signable p (Data.CommitSU p u)
         , Signable p (Data.SUHash p d, Data.Confidence, VKey p)
         ) => STS (GENAPPROVAL p u d) where

  type Environment (GENAPPROVAL p u d) = Env p d

  type State (GENAPPROVAL p u) = St p u d

  type Signal (GENAPPROVAL p u) = Data.SUPayload p u

  -- | GenApproval phase failures
  data PredicateFailure (GENAPPROVAL p u)
    = SUAlreadySubmitted (Data.SU p u)
    | NoSUToReveal (Data.SU p u)
    | SUAlreadyRevealed (Data.SU p u)
    | InvalidAuthor (VKey p)
    | NoStableAndCommittedSU (Data.SU p u) (WhenSubmittedSUs p)
    | InvalidVoter (VKey p)
    | VoteNotForActiveSU (Data.SUHash p u)
    | VotingPeriodEnded (Data.SUHash p u) Slot
    | CommitSignatureDoesNotVerify
    | VoteSignatureDoesNotVerify
    deriving (Eq, Show)

  initialRules = [ pure $! mempty ]

  transitionRules = [
    do
      TRC ( Env { k
                , currentSlot
                , aSUs
                , stakeDist
                }
          , st@St { subSUs
                  , wsSUs
                  , wrSUs
                  , sudb
                  , ballots
                  }
          , sig
          ) <- judgmentContext
      case sig of
        SubmitSU sucom su -> do
          hash (Data.authorSUcom sucom) ∈ dom stakeDist
            ?! InvalidAuthor (Data.authorSUcom sucom)
          Data.commitSU sucom ∉ dom subSUs ?! SUAlreadySubmitted su

          verify (Data.authorSUcom sucom) (Data.commitSU sucom) (Data.sigSUcom sucom) ?!
            CommitSignatureDoesNotVerify

          pure $! st { wsSUs = wsSUs ⨃ [(Data.commitSU sucom, currentSlot)]
                     , subSUs = subSUs ⨃ [(Data.commitSU sucom, su)]
                     }

        RevealSU su -> do
          hash (Data.authorSU su) ∈ dom stakeDist ?! InvalidAuthor (Data.authorSU su)
          (Data.calcCommit sip) ∈ dom subsips ?! NoSIPToReveal sip

          su ∉ range sudb ?! SUAlreadyRevealed su

          -- The Revealed SU must correspond to a stable Commited SU.
          -- Restrict the range of wsSUs to values less or equal than
          -- @currentSlot - 2k@
          Data.calcCommit sip
            ∈ dom (wsSUs ▷<= (currentSlot -. (2 *. k)))
            ?! NoStableAndCommittedSU su wsSUs

          pure st { wsSUs = Set.singleton (Data.calcCommit sip) ⋪ wsSUs
                  , wrSUs = wrSUs ⨃ [(Data.hashSU su, currentSlot)]
                  , sudb = sudb ⨃ [(Data.hashSU su, su)]
                  }

        Vote voteForSIP -> do
            -- voter must be a stakeholder
            hash (Data.voter voteForSIP) ∈ dom stakeDist ?!
              InvalidVoter (Data.voter voteForSIP)

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
    someStakeDist <- Gen.stakeDist someParticipants
    let env = Env { k = someK
                  , currentSlot = someCurrentSlot
                  , asips = mempty
                  , participants = someParticipants
                  , stakeDist = someStakeDist
                  }
    pure env

  sigGen
    _traceGenEnv
    Env{ k, currentSlot, asips, participants }
    St{ wssips, wrsips, subsips } =
      case Set.toList $ range $ stableCommits ◁ subsips of
        [] ->
          -- There are no stable commits

          -- Check if there are active sips whose voting end
          -- has not been reached yet.
          if Set.empty == dom (asips ▷>= currentSlot)
            then
            --, so we can only generate a submission.
            submissionGen
            else
              -- we can also generate votes
              QC.frequency [ (10, submissionGen)
                           , (90, voteGen asips currentSlot)
                           ]
        xs ->
          if Set.empty == dom (asips ▷>= currentSlot)
            then
              -- We use a 50/50 submission to revelation ratio. This can be changed
              -- if necessary.
              QC.frequency [ (1, submissionGen)
                           , (1, revelationGen xs)
                           ]
            else
              -- votes are much more frequent events than
              -- the submission or revealing of SIPs
              QC.frequency [ (5, submissionGen)
                           , (5, revelationGen xs)
                           , (90, voteGen asips currentSlot)
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
                    <*> (SlotCount <$> QC.choose (5, 30))
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

      voteGen
        :: ActiveSIPs Mock
        -> Slot
        -> QC.Gen (IdeationPayload Mock)
      voteGen actsips currSlot =
        do
          (voter, voterSkey) <- QC.elements $ toList participants
          -- We promote a bit more the positive votes because we want
          -- to have a good percent of approved SIPs, since only approvals
          -- take us to the next phase in the lifecycle
          confidence <- QC.frequency [ (60, pure $ Data.For)
                                     , (20, pure $ Data.Against)
                                     , (20, pure $ Data.Abstain)
                                     ]

          -- Choose among active sips whose voting period is still open
          sipHash <- QC.elements $ Set.toList $ dom (actsips ▷>= currSlot)
          let voterSig = sign (sipHash, confidence, voter) voterSkey
          pure $ Vote $ Data.VoteForSIP sipHash confidence voter voterSig


  -- It doesn't seem plausible that the @IdeationPayload@ can be shrunk in a
  -- meaningful way. If we shrink the SIP, we have to modify the commit, but
  -- then all the ballots will refer to invalid commits.
  --
  -- With this trace generation framework it might not be possible to get good
  -- shrinks for these signals.
  shrinkSignal (Submit _sipc _sip) = []
  shrinkSignal (Reveal _sip) = []
  shrinkSignal (Vote _ballot) = []
