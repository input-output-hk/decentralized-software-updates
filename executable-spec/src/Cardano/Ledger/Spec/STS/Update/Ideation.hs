{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Cardano.Ledger.Spec.STS.Update.Ideation where

import           Control.Arrow ((&&&))
import           Data.Bimap (Bimap, (!))
import qualified Data.Bimap as Bimap
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           GHC.Generics (Generic)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Hedgehog.Range (constant)

import           Cardano.Crypto.Hash (HashAlgorithm, hash)

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, TRC (TRC), initialRules, judgmentContext,
                     transitionRules, (?!))
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)
import           Ledger.Core (Slot (Slot))

import           Cardano.Ledger.Spec.STS.Update.Data
                     (IdeationPayload (Reveal, Submit, Vote), SIP (SIP),
                     SIPData (SIPData), Commit, SIPCommit)
import           Cardano.Ledger.Spec.STS.Update.Data (author, VotingResult, VotingPeriod, BallotSIP)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data

import           Ledger.Core (dom, (∈), (∉))
import qualified Ledger.Core as Core

--------------------------------------------------------------------------------
-- Updates ideation phase
--------------------------------------------------------------------------------

-- | Ideation phase of system updates
data IDEATION hashAlgo

-- | Ideation phase state
--
data St hashAlgo
  = St
    { subsips :: !(Set (SIP hashAlgo))
      -- ^ These are the SIPs that we need to generate for the testing to
      -- take place. From these both the commited SIP's as well as the revealed SIPs
      -- will be created. This state is not part of the update protocol, it is used
      -- only for SIP generation purposes.
    , wssips :: !(Map (Commit hashAlgo) Slot)
      -- ^ When a SIP commitment was submitted

    , wrsips :: !(Map (Data.SIPHash hashAlgo) Slot)
    -- ^ When a SIP was revealed

    , ballots :: !(Map (Data.SIPHash hashAlgo) (Map Core.VKey (BallotSIP hashAlgo)))
      -- ^ This stores the valid ballots for each SIP and for each voter

    , voteResultSIPs :: !(Map (Data.SIPHash hashAlgo) VotingResult)
      -- ^ This records the current voting result for each SIP
    }
  deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup (St hashAlgo)
  deriving Monoid via GenericMonoid (St hashAlgo)

-- Environmnet of the Ideation phase
data Env hashAlgo
  = Env
    { currentSlot :: !Slot
      -- ^ The current slot in the blockchain system
    , asips :: !(Map (Data.SIPHash hashAlgo) Slot)
    -- ^ When a SIP will not be active any more
    -- (i.e., end of open for voting period)
    , participants :: Bimap Core.VKey Core.SKey
      -- ^ The set of stakeholders (i.e., participants), identified by their signing
      -- and verifying keys.
      -- There is a one-to-one correspondence between the signing and verifying keys, hence
      -- the use of 'Bimap'
    }
  deriving (Eq, Show, Generic)



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
    | SIPFailedToBeRevealed (Data.SIP hashAlgo)
    | InvalidVoter Core.VKey
    | VoteNotForActiveSIP (Data.SIPHash hashAlgo)
    | VotingPeriodEnded (Data.SIPHash hashAlgo) Slot
    deriving (Eq, Show)

  initialRules = [ pure $! mempty ]

  transitionRules = [
    do
      TRC ( Env { currentSlot, asips, participants }
          , st@St { subsips
                  , wssips
                  , wrsips
                  , ballots
                  , voteResultSIPs
                  }
          , sig
          ) <- judgmentContext
      case sig of
        Submit sipc sip -> do
          author sip ∈ dom participants ?! InvalidAuthor (author sip)
          sip ∉ subsips ?! SIPAlreadySubmitted sip

          -- TODO: Add verification of signature inside SIPCommit

          pure $! st { wssips = Map.insert (Data.commit sipc) (currentSlot) wssips
                     , subsips = Set.insert sip subsips
                     }

        Reveal sip -> do
          author sip ∈ dom participants ?! InvalidAuthor (author sip)
          sip ∈ subsips ?! NoSIPToReveal sip
          -- TODO: Revealed SIP must belong to stable submitted SIPs

          (Data.sipHash sip) ∉ (dom wrsips) ?! SIPAlreadyRevealed sip
          --(Data.calcCommit sip) ∈ (dom wssips) ?! SIPFailedToBeRevealed sip

          -- The Revealed SIP must correspond to a stable Commited SIP
          (Data.calcCommit sip) ∈ (dom (wssips ▷ (currentSlot -. SlotCount (2*k)))) ?! SIPFailedToBeRevealed sip

          pure st { subsips = Set.delete sip subsips
                    wssips = Map.delete (Data.calcCommit sip) wssips
                  , wrsips = Map.insert (Data.sipHash sip) currentSlot wrsips
                  }

        Vote ballot -> do
            -- voter must be a stakeholder
            (Data.voter ballot) ∈ dom participants ?!
              InvalidVoter (Data.voter ballot)

            -- SIP must be an active SIP
            -- I.e., it must belong to the set of stable revealed SIPs
            (Data.votedsipHash ballot) ∈ (dom asips) ?!
              VoteNotForActiveSIP (Data.votedsipHash ballot)

            -- The Voting period end for this SIP must not have been reached yet.
            currentSlot <= asips!(Data.votedsipHash ballot) ?!
              VotingPeriodEnded (Data.votedsipHash ballot) currentSlot

            -- TODO: Signature of the vote must be verified

            -- Update State
              -- Add ballot to the state of valid ballots for this SIP
                -- If the voter has voted again, then replace his old vote with the new one
            pure $ st { ballots =
                          -- Are there any votes for this SIP yet?
                          if (Data.votedsipHash ballot) ∈ dom ballots
                            then
                              -- insert will overwrite the value part of the Map if the key exists
                              Map.insert
                                (Data.votedsipHash ballot)
                                (Map.insert
                                    (Data.voter ballot)
                                    ballot
                                    (ballots ! (Data.votedsipHash ballot))
                                )
                                ballots
                            else
                              Map.insert
                                (Data.votedsipHash ballot)
                                (Map.fromList [(Data.voter ballot, ballot)])
                                ballots
                      }
    ]


instance HashAlgorithm hashAlgo => HasTrace (IDEATION hashAlgo) where

  envGen _traceLength =
    Env <$> currentSlotGen
        -- TODO: for now we generate a constant set of keys. We need to update the
        -- 'HasTrace' class so that 'trace' can take parameter of an associated
        -- type, so that each STS can decide which parameters are relevant for its
        -- traces.
        <*> (pure
             $! Bimap.fromList
             $  fmap (Core.vKey &&& Core.sKey)
             $  fmap Core.keyPair
             $  fmap Core.Owner $ [0 .. 10])
        <*> closedVotingPeriodsGen
    where
      currentSlotGen = Slot <$> Gen.integral (Range.constant 0 100)
      -- TODO: generate a realistic Map
      closedVotingPeriodsGen = pure $ Map.empty

  -- For now we ignore the predicate failure we might need to provide (if any).
  -- We're interested in valid traces only at the moment.
  sigGen
    Env {participants}
    St { subsips } = do
      owner <- newOwner
      sipMData <- newSIPMetadata
      sipData <- newSipData sipMData
      sipHash <- newSipHash sipData
      salt <- newSalt
      -- generate the new SIP and pass it to generateASubmission "by value"
      -- otherwise you get non-deterministic SIP!
      newsip <- newSIP owner sipHash salt sipData
      case Set.toList subsips of
        [] ->
          generateASubmission newsip owner
        xs ->
          -- TODO: determine submission to revelation ration (maybe 50/50 is fine...)
          Gen.frequency [ (1, generateASubmission newsip owner)
                        , (1, generateARevelation xs)
                        ]
      where
        newOwner =
          Gen.element
          $ Set.toList
          $ dom participants

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

        newSIP newowner nsipHash nsalt nsipData =
          (SIP)
            <$> pure nsipHash
            <*> pure newowner
            <*> pure nsalt
            <*> pure nsipData

        -- Generate a submission taking a participant that hasn't submitted a proposal yet
        generateASubmission nsip owner = do
          (Submit)
            <$>
              ((Data.SIPCommit) <$> newCommit <*> (pure owner) <*> newSignature)
            <*>
              (pure nsip)
          where
            newSignature = (Core.sign) <$> skey <*> newCommit

            newCommit = fmap (Data.calcCommit) (pure nsip)

            -- Do a Bimap lookup to get the sk from the vk
            skey = (!) <$>  (pure participants) <*> (pure owner)

        generateARevelation subsipsList =
          fmap Reveal $ Gen.element subsipsList
