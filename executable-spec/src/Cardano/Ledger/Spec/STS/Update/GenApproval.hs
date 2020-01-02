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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Ledger.Spec.STS.Update.GenApproval where

import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import qualified Data.Set as Set
import           Data.Text as T
import           GHC.Exts (toList)
import           GHC.Generics (Generic)
import qualified Test.QuickCheck as QC
import           Data.Typeable (Typeable, typeOf)
import           Data.AbstractSize (HasTypeReps, typeReps)

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, TRC (TRC), initialRules, judgmentContext,
                     transitionRules, (?!))
import           Ledger.Core (dom, (∈), (∉), (▷<=), (-.), (*.), (⨃), (⋪), range, (◁)
                             , Slot, SlotCount (SlotCount), BlockCount, (▷>=))
import           Cardano.Binary (ToCBOR (toCBOR), encodeInt, encodeListLen)

import qualified Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen

import qualified Cardano.Ledger.Generators.QuickCheck as Gen
import           Cardano.Ledger.Spec.Classes.Hashable (Hashable, hash, HasHash, Hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme ( Signature
                                                              , sign
                                                              , SKey
                                                              , VKey
                                                              , HasSigningScheme
                                                              , verify
                                                              , Signable
                                                              )
import           Cardano.Ledger.Spec.Classes.Indexed2 ((!))
import           Cardano.Ledger.Spec.State.ActiveSUs (ActiveSUs)
import           Cardano.Ledger.Spec.State.BallotSUs (BallotSUs, updateBallot)
import           Cardano.Ledger.Spec.State.WhenRevealedSUs (WhenRevealedSUs)
import           Cardano.Ledger.Spec.State.WhenSubmittedSUs (WhenSubmittedSUs)
import           Cardano.Ledger.Spec.State.Participants (Participants)
import           Cardano.Ledger.Spec.State.RevealedSUs (RevealedSUs)
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import           Cardano.Ledger.Spec.State.SubmittedSUs (SubmittedSUs)
import           Cardano.Ledger.Spec.STS.Update.Data
                     (SIP (SIP),
                     SIPData (SIPData))
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.Classes.IsSUCommit ( SUCommit
                                                        , CommitSU
                                                        , IsSUCommit
                                                        , authorSUcom
                                                        , hashSUCommit
                                                        , sigSUcom
                                                        , calcCommitSU
                                                        )
import qualified Cardano.Ledger.Spec.Classes.IsSUCommit as IsSUCommit
import           Cardano.Ledger.Spec.Classes.IsSU (IsSU, SU, SUHash, authorSU, hashSU)
import qualified Cardano.Ledger.Spec.Classes.IsSU as IsSU
import           Cardano.Ledger.Spec.Classes.IsVoteForSU ( IsVote
                                                         , IsVoteForSU
                                                         , votedSUHash
                                                         , confidenceSU
                                                         , voterSU
                                                         , voterSigSU
                                                         )
import           Cardano.Ledger.Spec.State.BallotSUs (BallotSUs)
import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import Cardano.Ledger.Test.Mock (Mock)

--------------------------------------------------------------------------------
-- This a generic approval STS to be utilized by both
-- the Ideation and Approval STSs.
--------------------------------------------------------------------------------

-- | Software Update signals
-- p: hashing and signing algorithm
-- u: type of software update
data SUPayload u p
  = SubmitSU (SUCommit u p) (SU u p)
  | RevealSU (SU u p)
  | VoteSU (IsVote u p)
  deriving (Generic)

deriving instance ( Hashable p
                  , HasSigningScheme p
                  , Show u
                  , Show (SUCommit p u)
                  , Show (SU p u)
                  , Show (IsVote p u)
                  ) => Show (SUPayload p u)

deriving instance ( Typeable p
                  , Typeable u
                  , HasTypeReps p
                  , HasTypeReps u
                  , HasTypeReps (SU u p)
                  , HasTypeReps (SUHash u p)
                  , HasTypeReps (SUCommit u p)
                  , HasTypeReps (IsVote u p)
                  ) => HasTypeReps (SUPayload u p)

--------------------------------------------------------------------------------
-- Sized instances
--------------------------------------------------------------------------------

instance ( Typeable p
         , Typeable u
         , HasTypeReps p
         , HasTypeReps u
         , HasTypeReps (VKey p)
         , HasTypeReps (SUPayload p u)
         ) => Sized (SUPayload p u) where
  costsList suPayload = [(typeOf suPayload, 10)]

-- | A Generic Approval STS.
-- Implements a generic apporval STS to be instantiated by the
-- `Ideation` STS and the `Approval` STS.
-- It is polymorphic in the hashing and signing algorithm (parameter @p@) and in
-- the type of the software update (`SIP` or `UP`) (parameter @u@)
data GENAPPROVAL u p

-- Environmnet of the ApprovalGen phase
data Env u p
  = Env
    { k :: !BlockCount
      -- ^ Chain stability parameter.
    , currentSlot :: !Slot
      -- ^ The current slot in the blockchain system
    , aSUs :: !(ActiveSUs u p)
    , participants :: !(Participants p)
    , stakeDist :: !(StakeDistribution p)
    }
  deriving (Generic)

deriving instance (Hashable p, HasSigningScheme p, Show (SUHash u p)) => Show (Env u p)


-- | Generic Approval state
--
data St u p
  = St
    { subSUs :: !(SubmittedSUs u p)
    , wsSUs :: !(WhenSubmittedSUs u p)
    , wrSUs :: !(WhenRevealedSUs u p)
    , sudb :: !(RevealedSUs u p)
    , ballots :: !(BallotSUs u p)
    }
  deriving (Generic)
 -- deriving Semigroup via GenericSemigroup (St u p)
 -- deriving Monoid via GenericMonoid (St u p)

deriving instance (Hashable p, Show (CommitSU u p), Show (SU u p), Show (SUHash u p)) => Show (St u p)
deriving instance (Hashable p, Ord (CommitSU u p)) => Semigroup (St u p)
deriving instance (Hashable p, Ord (CommitSU u p)) => Monoid (St u p)

instance ( Hashable p
         , HasHash p (IsSU.SUData u p)
         , HasHash p u
         , HasHash p (IsSU.SU u p)
         , HasHash p (Int, VKey p, Hash p (IsSU.SU u p))
         , HasHash p (VKey p) -- needed to bring the 'Ord' instance for 'SU'.
         , HasSigningScheme p
         , Signable p (IsSUCommit.CommitSU u p)
         , Signable p (IsSU.SUHash u p, Data.Confidence, VKey p)
         ) => STS (GENAPPROVAL u p) where

  type Environment (GENAPPROVAL u p) = Env u p

  type State (GENAPPROVAL u p) = St u p

  type Signal (GENAPPROVAL u p) = SUPayload u p

  -- | GenApproval phase failures
  data PredicateFailure (GENAPPROVAL u p)
    = SUAlreadySubmitted (IsSU.SU u p)
    | NoSUToReveal (IsSU.SU u p)
    | SUAlreadyRevealed (IsSU.SU u p)
    | InvalidAuthor (VKey p)
    | NoStableAndCommittedSU (IsSU.SU u p) (WhenSubmittedSUs u p)
    | InvalidVoter (VKey p)
    | VoteNotForActiveSU (IsSU.SUHash u p)
    | VotingPeriodEnded (IsSU.SUHash u p) Slot
    | CommitSignatureDoesNotVerify
    | VoteSignatureDoesNotVerify
  -- deriving (Eq, Show)

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
          hash (authorSUcom @p $ sucom @p) ∈ dom stakeDist
            ?! InvalidAuthor (authorSUcom @p $ sucom @p)
          hashSUCommit sucom ∉ dom subSUs ?! SUAlreadySubmitted su

          verify (authorSUcom sucom) (hashSUCommit sucom) (sigSUcom sucom) ?!
            CommitSignatureDoesNotVerify

          pure $! st { wsSUs = wsSUs ⨃ [(hashSUCommit sucom, currentSlot)]
                     , subSUs = subSUs ⨃ [(hashSUCommit sucom, su)]
                     }

        RevealSU su -> do
          hash (authorSU su) ∈ dom stakeDist ?! InvalidAuthor (authorSU su)
          (calcCommitSU su) ∈ dom subSUs ?! NoSUToReveal su

          su ∉ range sudb ?! SUAlreadyRevealed su

          -- The Revealed SU must correspond to a stable Commited SU.
          -- Restrict the range of wsSUs to values less or equal than
          -- @currentSlot - 2k@
          calcCommitSU su
            ∈ dom (wsSUs ▷<= (currentSlot -. (2 *. k)))
            ?! NoStableAndCommittedSU su wsSUs

          pure st { wsSUs = Set.singleton (calcCommitSU su) ⋪ wsSUs
                  , wrSUs = wrSUs ⨃ [(hashSU su, currentSlot)]
                  , sudb = sudb ⨃ [(hashSU su, su)]
                  }

        VoteSU vote -> do
            -- voter must be a stakeholder
            hash (voterSU vote) ∈ dom stakeDist ?!
              InvalidVoter (voterSU vote)

            verify
              (voterSU vote)
              ( votedSUHash vote
              , confidenceSU vote
              , voterSU vote
              )
              (voterSigSU vote)
              ?!
              VoteSignatureDoesNotVerify

            -- SU must be an active SU, i.e. it must belong to the set of
            -- stable revealed SUs
            votedSUHash vote ∈ dom aSUs ?!
              VoteNotForActiveSU (votedSUHash vote)

            -- The end of the voting period for this SU must not have been
            -- reached yet.
            currentSlot <= aSUs ! votedSUHash vote ?!
              VotingPeriodEnded (votedSUHash vote) currentSlot

            pure $ st { ballots = updateBallot ballots vote }
    ]


deriving instance ( Eq (SU u p)
                  , Eq (VKey p)
                  , Eq (CommitSU u p)
                  , Eq (SUHash u p)
                  ) => Eq (PredicateFailure (GENAPPROVAL u p))
deriving instance ( Show (SU u p)
                  , Show (VKey p)
                  , Show (CommitSU u p)
                  , Show (SUHash u p)
                  ) => Show (PredicateFailure (GENAPPROVAL u p))


instance ( ToCBOR (IsSU.SUData u Mock)
         ) => STS.Gen.HasTrace (GENAPPROVAL u Mock) a where

  envGen :: (Ord (SUHash u Mock)) => a -> QC.Gen (Env u Mock)
  envGen _traceGenEnv
    = do
    someK <- Gen.k
    someCurrentSlot <- Gen.currentSlot
    someParticipants <- Gen.participants
    someStakeDist <- Gen.stakeDist someParticipants
    let env = Env { k = someK
                  , currentSlot = someCurrentSlot
                  , aSUs = mempty
                  , participants = someParticipants
                  , stakeDist = someStakeDist
                  }
    pure env

  sigGen
    _traceGenEnv
    Env{ k, currentSlot, aSUs, participants }
    St{ wsSUs, wrSUs, subSUs } =
      case Set.toList $ range $ stableCommits ◁ subSUs of
        [] ->
          -- There are no stable commits

          -- Check if there are active su whose voting end
          -- has not been reached yet.
          if Set.empty == dom (aSUs ▷>= currentSlot)
            then
            --, so we can only generate a submission.
            submissionGen
            else
              -- we can also generate votes
              QC.frequency [ (10, submissionGen)
                           , (90, voteGen aSUs currentSlot)
                           ]
        xs ->
          if Set.empty == dom (aSUs ▷>= currentSlot)
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
                           , (90, voteGen aSUs currentSlot)
                           ]
    where
      stableCommits = dom (wsSUs ▷<= (currentSlot -. (2 *. k)))
      submissionGen = do
        -- WARNING: suchThat can be very inefficient if this condition fails often.
        (su, skeyAuthor) <- suGen `QC.suchThat` ((`Set.notMember` range subSUs) . fst)
        pure $! mkSubmission skeyAuthor su
          where
            mkSubmission :: SKey Mock -> SU u Mock -> SUPayload u Mock
            mkSubmission skey su = SubmitSU ( suCommitGen
                                              (calcCommitSU su)
                                              (authorSU su)
                                              (sign (calcCommitSU su) skey)
                                            ) su

      revelationGen subSUsList = do
        -- Problem! 'QC.suchThat' will loop forever if it cannot find a
        -- revelation that satisfies the given predicate!
        su <- QC.elements subSUsList
        if hashSU su ∉ dom wrSUs
          then pure $! RevealSU su
          else submissionGen

      voteGen
        :: ActiveSUs u Mock
        -> Slot
        -> QC.Gen (SUPayload u Mock)
      voteGen actSUs currSlot =
        do
          (voter, voterSkey) <- QC.elements $ toList participants
          -- We promote a bit more the positive votes because we want
          -- to have a good percent of approved SIPs, since only approvals
          -- take us to the next phase in the lifecycle
          confidence <- QC.frequency [ (60, pure $ Data.For)
                                     , (20, pure $ Data.Against)
                                     , (20, pure $ Data.Abstain)
                                     ]

          -- Choose among active SUs whose voting period is still open
          suHash <- QC.elements $ Set.toList $ dom (actSUs ▷>= currSlot)
          let voterSig = sign (suHash, confidence, voter) voterSkey
          pure $ VoteSU $ suVoteGen suHash confidence voter voterSig
        --  pure $ Vote $ Data.VoteForSIP sipHash confidence voter voterSig


  -- It doesn't seem plausible that the @IdeationPayload@ can be shrunk in a
  -- meaningful way. If we shrink the SIP, we have to modify the commit, but
  -- then all the ballots will refer to invalid commits.
  --
  -- With this trace generation framework it might not be possible to get good
  -- shrinks for these signals.
  shrinkSignal (SubmitSU _suc _su) = []
  shrinkSignal (RevealSU _su) = []
  shrinkSignal (VoteSU _ballot) = []


class (Hashable p, IsSU u p) => SUGen u p where
  suGen :: QC.Gen (SU u p, SKey p)

instance (Hashable p) => SUGen (Data.SIP p) p where
  suGen = sipGen

instance (Hashable p) => SUGen (Data.UP p) p where
  suGen = upGen

-- | Generate a `data.SIP`
sipGen :: (Hashable p) => QC.Gen (Data.SIP p, SKey p)
sipGen = do
  (vkeyAuthor, skeyAuthor) <- QC.elements $ toList participants
  sipMData <- sipMetadataGen
  sipData <- sipDataGen sipMData
  let sipHash = Data.SIPHash $ hash sipData
  salt <- Gen.bounded 2
  pure $! (Data.SIP sipHash vkeyAuthor salt sipData, skeyAuthor)
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

-- Generate a `data.UP`
upGen :: (Hashable p) => QC.Gen (Data.UP p, SKey p)
upGen  = undefined

class (Hashable p, IsSUCommit u p, IsSU su p) => SUCommitGen u p su where
  suCommitGen :: CommitSU u p -> VKey p -> (Signature p (CommitSU u p)) -> SUCommit u p

instance (Hashable p) => SUCommitGen (Data.SIPCommit p) p (Data.SIP p) where
  suCommitGen commit author signature = Data.SIPCommit commit author signature

instance (Hashable p) => SUCommitGen (Data.UPCommit p) p (Data.UP p) where
  suCommitGen commit author signature = Data.UPCommit commit author signature


class (Hashable p, IsVoteForSU u p, IsSU u p) => SUVoteGen u p where
  suVoteGen
    :: SUHash u p
    -> Data.Confidence
    -> VKey p
    -> Signature p (SUHash u p, Data.Confidence, VKey p)
    -> IsVote u p

instance (Hashable p) => SUVoteGen (Data.SIP p) p where
  suVoteGen sipHash confidence voter voterSig = Data.VoteForSIP sipHash confidence voter voterSig

instance (Hashable p) => SUVoteGen (Data.UP p) p where
  suVoteGen upHash confidence voter voterSig = Data.VoteForUP upHash confidence voter voterSig
