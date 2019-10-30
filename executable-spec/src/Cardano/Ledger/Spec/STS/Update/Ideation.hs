{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Cardano.Ledger.Spec.STS.Update.Ideation where

import Data.Text as T
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map, (!), (!?))
import           Data.Maybe (fromMaybe)
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           GHC.Generics (Generic)
import qualified Data.Set as Set

import qualified Test.QuickCheck as QC

import qualified Hedgehog.Gen as Gen
import           Hedgehog.Range (constant)

import           Cardano.Crypto.Hash (HashAlgorithm, hash)

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, TRC (TRC), initialRules, judgmentContext,
                     transitionRules, (?!))
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)
import           Ledger.Core (Slot, BlockCount)
import           Ledger.Core (dom, (∈), (∉), (▷<=), (-.), (*.), (⨃), (⋪), range, (◁))
import qualified Ledger.Core as Core

import qualified Control.State.Transition.Trace.Generator.QuickCheck as Trace.QC

import           Cardano.Ledger.Generators (kGen, participantsGen, currentSlotGen)
import qualified Cardano.Ledger.Generators.QuickCheck as Gen.QC
import           Cardano.Ledger.Spec.STS.Update.Data
                     (IdeationPayload (Reveal, Submit, Vote), SIP (SIP),
                     SIPData (SIPData), Commit)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data


--------------------------------------------------------------------------------
-- Updates ideation phase
--------------------------------------------------------------------------------

-- | Ideation phase of system updates
data IDEATION hashAlgo

-- Environmnet of the Ideation phase
data Env hashAlgo
  = Env
    { k :: !BlockCount
      -- ^ Chain stability parameter.
    , currentSlot :: !Slot
      -- ^ The current slot in the blockchain system
    , asips :: !(Map (Data.SIPHash hashAlgo) Slot)
      -- ^ Active SIP's. The slot in the range (of the map) determines when the
      -- voting period will end.
    , participants :: !(Bimap Core.VKey Core.SKey)
      -- ^ The set of stakeholders (i.e., participants), identified by their signing
      -- and verifying keys.
      --
      -- There is a one-to-one correspondence between the signing and verifying keys, hence
      -- the use of 'Bimap'
      --
      -- TODO: DISCUSS: It seems we need this only for the generators. We might want to
      -- remove this from the STS.
    }
  deriving (Eq, Show, Generic)

-- | Ideation phase state
--
data St hashAlgo
  = St
    { subsips :: !(Map (Commit hashAlgo) (SIP hashAlgo))
      -- ^ These are the SIPs that we need to generate for the testing to
      -- take place. From these both the commited SIP's as well as the revealed SIPs
      -- will be created. This state is not part of the update protocol, it is used
      -- only for SIP generation purposes.
    , wssips :: !(Map (Commit hashAlgo) Slot)
      -- ^ When a SIP commitment was submitted

    , wrsips :: !(Map (Data.SIPHash hashAlgo) Slot)
      -- ^ When a SIP was revealed

    , sipdb :: !(Map (Data.SIPHash hashAlgo) (SIP hashAlgo))
      -- ^ Local SIP state. Includes all revealed SIPs from the
      -- beginning of time

    , ballots :: !(Map (Data.SIPHash hashAlgo) (Map Core.VKey Data.Confidence))
      -- ^ Stores the valid ballots for each SIP and for each voter

      -- TODO: I think we should use key hashes here as well.
      --
      -- DISCUSS: by using this representation I think we're using more data,
      -- since we have to store the confidence as well. We could avoid this by
      -- having three maps, say @for@, @against@, and @abstain@. However a big
      -- advantage of this construction is that any given key can only vote for
      -- a single proposal and confidence by construction (if we use three maps
      -- we have to maintain this as an invariant).

    }
  deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup (St hashAlgo)
  deriving Monoid via GenericMonoid (St hashAlgo)


instance HashAlgorithm hashAlgo => STS (IDEATION hashAlgo) where

  type Environment (IDEATION hashAlgo) = Env hashAlgo

  type State (IDEATION hashAlgo) = St hashAlgo

  type Signal (IDEATION hashAlgo) = IdeationPayload hashAlgo

  -- | IDEATION phase failures
  data PredicateFailure (IDEATION hashAlgo)
    = SIPAlreadySubmitted (Data.SIP hashAlgo)
   -- | SIPSubmittedAlreadyRevealed (Data.SIP hashAlgo)
    | NoSIPToReveal (Data.SIP hashAlgo)
    | SIPAlreadyRevealed (Data.SIP hashAlgo)
    | InvalidAuthor Core.VKey
    | NoStableAndCommittedSIP (Data.SIP hashAlgo) (Map (Commit hashAlgo) Slot)
    | InvalidVoter Core.VKey
    | VoteNotForActiveSIP (Data.SIPHash hashAlgo)
    | VotingPeriodEnded (Data.SIPHash hashAlgo) Slot
    deriving (Eq, Show)

  initialRules = [ pure $! mempty ]

  transitionRules = [
    do
      TRC ( Env { k
                , currentSlot
                , asips
                , participants
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
          Data._author sipc ∈ dom participants ?! InvalidAuthor (Data.author sip)
          Data.commit sipc ∉ dom subsips ?! SIPAlreadySubmitted sip

          -- TODO: Add verification of signature inside SIPCommit

          pure $! st { wssips = wssips ⨃ [(Data.commit sipc, currentSlot)]
                     , subsips = subsips ⨃ [(Data.commit sipc, sip)]
                     }

        Reveal sip -> do
          Data.author sip ∈ dom participants ?! InvalidAuthor (Data.author sip)
          (Data.calcCommit sip) ∈ dom subsips ?! NoSIPToReveal sip

          sip ∉ range sipdb ?! SIPAlreadyRevealed sip

          -- The Revealed SIP must correspond to a stable Commited SIP.
          -- Restrict the range of wssips to values less or equal than
          -- @currentSlot - 2k@
          Data.calcCommit sip
            ∈ dom (wssips ▷<= (currentSlot -. (2 *. k)))
            ?! NoStableAndCommittedSIP sip wssips

          pure st { wssips = Set.singleton (Data.calcCommit sip) ⋪ wssips -- TODO: domain/range restriction should be able to take a foldable.
                  , wrsips = wrsips ⨃ [(Data.sipHash sip, currentSlot)]
                  , sipdb = sipdb ⨃ [(Data.sipHash sip, sip)]
                  }

        Vote ballot -> do
            -- voter must be a stakeholder
            Data.voter ballot ∈ dom participants ?!
              InvalidVoter (Data.voter ballot)

            -- SIP must be an active SIP, i.e. it must belong to the set of
            -- stable revealed SIPs
            Data.votedsipHash ballot ∈ dom asips ?!
              VoteNotForActiveSIP (Data.votedsipHash ballot)

            -- The end of the voting period for this SIP must not have been
            -- reached yet.
            currentSlot <= asips ! Data.votedsipHash ballot ?!
              VotingPeriodEnded (Data.votedsipHash ballot) currentSlot

            -- TODO: Signature of the vote must be verified

            let
              hsip = Data.votedsipHash ballot
              hsipBallots = fromMaybe Map.empty $ ballots !? hsip
              hsipBallots' = hsipBallots ⨃ [(Data.voter ballot, Data.confidence ballot)]

            -- Note that if the voter has voted again, then replace his old vote
            -- with the new one.
            pure $ st { ballots = ballots ⨃ [(hsip, hsipBallots')] }
    ]

instance HashAlgorithm hashAlgo => HasTrace (IDEATION hashAlgo) where

  envGen _traceLength
    = Env
    <$> kGen
    <*> currentSlotGen
    -- TODO: for now we generate a constant set of keys. We need to update the
    -- 'HasTrace' class so that 'trace' can take parameter of an associated
    -- type, so that each STS can decide which parameters are relevant for its
    -- traces.
    <*> asipsGen
    <*> participantsGen
    where
     -- TODO: generate a realistic Map
     --
     -- TODO: DISCUSS: dnadales: I don't think we can come up with a realistic
     -- set of SIP hashes here. I wonder if we can/need-to do any better than
     -- just returning an empty map.
     asipsGen = pure $ Map.empty

  -- For now we ignore the predicate failure we might need to provide (if any).
  -- We're interested in valid traces only at the moment.
  sigGen Env{ k, currentSlot, participants } St{ wssips, subsips } =
      let stableCommits = dom (wssips ▷<= (currentSlot -. (2 *. k))) in
      case Set.toList $ range $ stableCommits ◁ subsips of
        [] ->
          -- There are no stable commits, so we can only generate a submission.
          submissionGen
        xs ->
          -- TODO: determine submission to revelation ratio (maybe 50/50 is fine...)
          Gen.frequency [ (1, submissionGen)
                        , (1, revelationGen xs)
                        ]
      where
        submissionGen = do
          -- sip <- Gen.filter (`Set.notMember` ( (range subsips)
          --                                      `Set.union`
          --                                      (range sipdb)
          --                                    )

          --                   ) sipGen

          -- generate a new (never seen before) sip
          sip <- Gen.filter
                   (\s -> Data.calcCommit s `Set.notMember` (dom subsips))
                   sipGen

          pure $! mkSubmission sip
            where
              sipGen = do
                owner <- Gen.element $ Set.toList $ dom participants
                sipMData <- sipMetadataGen
                sipData <- sipDataGen sipMData
                let sipHash = Data.SIPHash $ hash sipData
                salt <- Gen.int (constant 0 100)
                pure $! SIP sipHash owner salt sipData
                  where
                    sipMetadataGen
                      = Data.SIPMetadata
                      <$> versionFromGen
                      <*> versionToGen
                      <*> Gen.element [Data.Impact, Data.NoImpact]
                      <*> Gen.subsequence [ Data.BlockSizeMax
                                          , Data.TxSizeMax
                                          , Data.SlotSize
                                          , Data.EpochSize
                                          ]
                      <*> Gen.element [Data.VPMin, Data.VPMedium, Data.VPLarge]
                      where
                        versionFromGen
                          =  (,)
                          <$> fmap Data.ProtVer word64Gen
                          <*> fmap Data.ApVer word64Gen

                        versionToGen = versionFromGen

                        word64Gen = Gen.word64 (constant 0 100)

                    sipDataGen sipMData = (SIPData) <$> (Data.URL <$> Gen.text (constant 1 20) Gen.alpha) <*> (pure sipMData)

              mkSubmission :: SIP hashAlgo -> IdeationPayload hashAlgo
              mkSubmission sip = Submit sipCommit sip
                where
                  sipCommit =
                    Data.SIPCommit commit (Data.author sip) sipCommitSignature
                    where
                      commit = Data.calcCommit sip
                      sipCommitSignature = Core.sign skey commit
                        where
                          skey = participants Bimap.! Data.author sip

        revelationGen subsipsList =
          fmap Reveal $! Gen.element subsipsList

instance
  HashAlgorithm hashAlgo
  => Trace.QC.HasTrace (IDEATION hashAlgo) () where

  envGen :: () -> QC.Gen (Env hashAlgo)
  envGen _traceGenEnv
    = do
    someK <- Gen.QC.kGen
    someCurrentSlot <- Gen.QC.currentSlotGen
    -- TODO: for now we generate a constant set of keys. We need to update the
    -- 'HasTrace' class so that 'trace' can take parameter of an associated
    -- type, so that each STS can decide which parameters are relevant for its
    -- traces.
    someParticipants <- Gen.QC.participantsGen
    let env = Env { k = someK
                  , currentSlot = someCurrentSlot
                  , asips = Map.empty
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
          -- TODO: determine submission to revelation ratio (maybe 50/50 is fine...)
          QC.frequency [ (1, submissionGen)
                       , (1, revelationGen xs)
                       ]
    where
      stableCommits = dom (wssips ▷<= (currentSlot -. (2 *. k)))
      submissionGen = do
        -- WARNING: suchThat can be very inefficient if this condition fails often.
        sip <- sipGen `QC.suchThat` (`Set.notMember` range subsips)
        pure $! mkSubmission sip
          where
            sipGen = do
              owner <- QC.elements $ Set.toList $ dom participants
              sipMData <- sipMetadataGen
              sipData <- sipDataGen sipMData
              let sipHash = Data.SIPHash $ hash sipData
              salt <- QC.choose (0, 100) -- TODO: we can choose smaller numbers as @salt@ value.
              pure $! SIP sipHash owner salt sipData
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

                      word64Gen = QC.choose (0, 100)

                  sipDataGen sipMData = SIPData <$> (Data.URL <$> urlText) <*> (pure sipMData)
                    where
                      urlText = do
                        n <- QC.choose (0, 20)
                        str <- QC.vectorOf n QC.arbitraryUnicodeChar
                        pure $! T.pack str

            mkSubmission :: SIP hashAlgo -> IdeationPayload hashAlgo
            mkSubmission sip = Submit sipCommit sip
              where
                sipCommit =
                  Data.SIPCommit commit (Data.author sip) sipCommitSignature
                  where
                    commit = Data.calcCommit sip
                    sipCommitSignature = Core.sign skey commit
                      where
                        skey = participants Bimap.! Data.author sip

      revelationGen subsipsList = do
        -- Problem! 'QC.suchThat' will loop forever if it cannot find a
        -- revelation that satisfies the given predicate!
        sip <- QC.elements subsipsList
        if Data.sipHash sip ∉ dom wrsips
          then pure $! Reveal sip
          else submissionGen


  shrinkSignal x = [x] -- For now we don't shrink the ideation payload
