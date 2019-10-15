{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Cardano.Ledger.Spec.STS.Update.Ideation where

import           Control.Arrow ((&&&))
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map, (!), (!?))
import           Data.Maybe (fromMaybe)
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           GHC.Generics (Generic)
import qualified Data.Set as Set

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Hedgehog.Range (constant)

import           Cardano.Crypto.Hash (HashAlgorithm, hash)

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, TRC (TRC), initialRules, judgmentContext,
                     transitionRules, (?!))
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)
import           Ledger.Core (Slot (Slot), BlockCount (BlockCount))
import           Ledger.Core (dom, (∈), (∉), (▷<=), (-.), (*.), (⨃), (⋫), (⋪), range, (◁))
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.STS.Update.Data
                     (IdeationPayload (Reveal, Submit, Vote), SIP (SIP),
                     SIPData (SIPData), Commit)
import           Cardano.Ledger.Spec.STS.Update.Data (author, VotingResult, Confidence)
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
    , participants :: Bimap Core.VKey Core.SKey
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

    , ballots :: !(Map (Data.SIPHash hashAlgo) (Map Core.VKey Confidence))
      -- ^ Stores the valid ballots for each SIP and for each voter

      -- TODO: I think we should use key hashes here as well.
      --
      -- DISCUSS: by using this representation I think we're using more data,
      -- since we have to store the confidence as well. We could avoid this by
      -- having three maps, say @for@, @against@, and @abstain@. However a big
      -- advantage of this construction is that any given key can only vote for
      -- a single proposal and confidence by construction (if we use three maps
      -- we have to maintain this as an invariant).

    , voteResultSIPs :: !(Map (Data.SIPHash hashAlgo) VotingResult)
      -- ^ Records the current voting result for each SIP
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
      TRC ( Env { k, currentSlot, asips, participants }
          , st@St { subsips
                  , wssips
                  , wrsips
                  , ballots
                  }
          , sig
          ) <- judgmentContext
      case sig of
        Submit sipc sip -> do
          author sip ∈ dom participants ?! InvalidAuthor (author sip)
          sip ∉ subsips ?! SIPAlreadySubmitted sip

          -- TODO: Add verification of signature inside SIPCommit

          pure $! st { wssips = wssips ⨃ [(Data.commit sipc, currentSlot)]
                     , subsips = subsips ⨃ [(Data.commit sipc, sip)]
                     }

        Reveal sip -> do
          author sip ∈ dom participants ?! InvalidAuthor (author sip)
          sip ∈ subsips ?! NoSIPToReveal sip
          -- TODO: Revealed SIP must belong to stable submitted SIPs

          Data.sipHash sip ∉ dom wrsips ?! SIPAlreadyRevealed sip

          -- The Revealed SIP must correspond to a stable Commited SIP.
          -- Restrict the range of wssips to values less or equal than
          -- @currentSlot - 2k@
          Data.calcCommit sip
            ∈ dom (wssips ▷<= (currentSlot -. (2 *. k)))
            ?! NoStableAndCommittedSIP sip wssips

          pure st { subsips = subsips ⋫ Set.singleton sip -- TODO: DISCUSS: not sure whether we need to delete this...
                  , wssips = Set.singleton (Data.calcCommit sip) ⋪ wssips -- TODO: domain/range restriction should be able to take a foldable.
                  , wrsips = Map.insert (Data.sipHash sip) currentSlot wrsips
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

            -- Update State
              -- Add ballot to the state of valid ballots for this SIP
              -- If the voter has voted again, then replace his old vote with the new one
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
    <*> (pure
         $! Bimap.fromList
         $  fmap (Core.vKey &&& Core.sKey)
         $  fmap Core.keyPair
         $  fmap Core.Owner $ [0 .. 10])
    where
     kGen = pure $ BlockCount 10 -- k security parameter
     currentSlotGen = Slot <$> Gen.integral (Range.constant 0 100)
      -- TODO: generate a realistic Map
     asipsGen = pure $ Map.empty

  -- For now we ignore the predicate failure we might need to provide (if any).
  -- We're interested in valid traces only at the moment.
  sigGen Env{ k, currentSlot, participants } St{ wssips, subsips } = do
      owner <- newOwner
      sipMData <- newSIPMetadata
      sipData <- newSipData sipMData
      sipHash <- newSipHash sipData
      salt <- newSalt
      -- generate the new SIP and pass it to generateASubmission "by value"
      -- otherwise you get non-deterministic SIP!
      newsip <- Gen.filter (`Set.notMember` range subsips) $ newSIP owner sipHash salt sipData
      let stableCommits = dom (wssips ▷<= (currentSlot -. (2 *. k)))
      case Set.toList $ range $ stableCommits ◁ subsips of
        [] ->
          generateASubmission newsip owner
        xs ->
          -- TODO: determine submission to revelation ratio (maybe 50/50 is fine...)
          Gen.frequency [ (1, generateASubmission newsip owner)
                        , (1, generateARevelation xs)
                        ]
      where
        newOwner = Gen.element $ Set.toList $ dom participants

        newSIPMetadata = (Data.SIPMetadata)
          <$> (
              ((,)) <$> (fmap (Data.ProtVer) $ Gen.word64 (constant 0 100))
                    <*> (fmap (Data.ApVer) $ Gen.word64 (constant 0 100))
              )
          <*> (
              ((,)) <$> (fmap (Data.ProtVer) $ Gen.word64 (constant 0 100))
                    <*> (fmap (Data.ApVer) $ Gen.word64 (constant 0 100))
              )
          <*> (Gen.element [Data.Impact, Data.NoImpact])
          <*> (Gen.element [[Data.BlockSizeMax], [Data.TxSizeMax], [Data.SlotSize], [Data.EpochSize]])
          <*> Gen.element [Data.VPMin, Data.VPMedium, Data.VPLarge]

        newSipData sipMData = (SIPData) <$> (Data.URL <$> Gen.text (constant 1 20) Gen.alpha) <*> (pure sipMData)

        newSalt = Gen.int (constant 0 100)

        newSipHash sipData = (Data.SIPHash . hash) <$>  (pure sipData)

        newSIP newowner nsipHash nsalt nsipData
          = SIP
          <$> pure nsipHash
          <*> pure newowner
          <*> pure nsalt
          <*> pure nsipData

        -- Generate a submission taking a participant that hasn't submitted a proposal yet
        generateASubmission nsip owner
          = Submit
          <$> ((Data.SIPCommit) <$> newCommit <*> pure owner <*> newSignature)
          <*> pure nsip
          where
            newSignature = (Core.sign) <$> skey <*> newCommit

            newCommit = fmap (Data.calcCommit) (pure nsip)

            -- Do a Bimap lookup to get the sk from the vk
            skey = pure $ participants Bimap.! owner

        generateARevelation subsipsList =
          fmap Reveal $ Gen.element subsipsList
