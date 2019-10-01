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
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Range (constant)
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           GHC.Generics (Generic)


import           Cardano.Crypto.Hash (HashAlgorithm, hash)

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, TRC (TRC), initialRules, judgmentContext,
                     transitionRules, (?!))
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import           Cardano.Ledger.Spec.STS.Update.Data
                     (IdeationPayload (Reveal, Submit, Vote), SIP (SIP),
                     SIPData (SIPData), Commit, SIPCommit)
import           Cardano.Ledger.Spec.STS.Update.Data (author, VotingResult, VotingPeriod, BallotSIP)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data

import qualified Data.Map.Strict as Map
import           Ledger.Core (dom, (∈), (∉))
import qualified Ledger.Core as Core

--------------------------------------------------------------------------------
-- Updates ideation phase
--------------------------------------------------------------------------------

-- | Ideation phase state
--
data St hashAlgo
  = St
    { commitedSIPs :: !(Map (Commit hashAlgo) (SIPCommit hashAlgo))
      -- ^ These are the encrypted SIPs that are submitted at the commit phase
      -- of an SIP submission
    , submittedSIPs :: !(Set (SIP hashAlgo))
      -- ^ These are the SIPs that we need to generate for the testing to
      -- take place. From these both the commitedSIP's as well as the revealedSIPs
      -- will be created. This state is not part of the update protocol, it is used
      -- only for SIP generation purposes.
    , revealedSIPs :: !(Set (SIP hashAlgo))
      -- ^ These are the revealed SIPs
    , ballotsForSIP :: !(Map (SIP hashAlgo) (Map Core.VKey (BallotSIP hashAlgo)))
      -- ^ This stores the valid ballots for each SIP and the voters
    , voteResultSIPs :: !(Map (SIP hashAlgo) VotingResult)
      -- ^ This records the current voting result for each SIP

      -- TODO: include this in the state of CHAINS and move it to the Ideation Env
    , openVotingPeriods :: !(Set VotingPeriod)
      -- ^ Records the open voting periods

      -- TODO: include this in the state of CHAINS and move it to the Ideation Env
    , closedVotingPeriods :: !(Set VotingPeriod)
  -- ^ Records the closed voting periods
    }
  deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup (St hashAlgo)
  deriving Monoid via GenericMonoid (St hashAlgo)


-- | Ideation phase of system updates
data IDEATION hashAlgo

instance HashAlgorithm hashAlgo => STS (IDEATION hashAlgo) where

  -- | The environment is the set of participants, identified by their signing
  -- and verifying keys.
  --
  -- There is a one-to-one correspondence the signing and verifying keys, hence
  -- the use of 'Bimap'
  type Environment (IDEATION hashAlgo) = Bimap Core.VKey Core.SKey

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
    | VoteNotForRevealedSIP (Data.SIP hashAlgo)
    deriving (Eq, Show)

  initialRules = [ pure $! mempty ]

  transitionRules = [
    do
      TRC ( participants
          , st@St { commitedSIPs, submittedSIPs, revealedSIPs, ballotsForSIP }
          , sig
          ) <- judgmentContext
      case sig of
        Submit sipc sip -> do
          author sip ∈ dom participants ?! InvalidAuthor (author sip)
          sip ∉ submittedSIPs ?! SIPAlreadySubmitted sip

          -- TODO: Add verification of signature inside SIPCommit

          pure $! st { commitedSIPs = Map.insert (Data.commit sipc) (sipc) commitedSIPs
                     , submittedSIPs = Set.insert sip submittedSIPs
                     -- TODO: if stabilization period has passed, then add commitSIP to stable commitedSIP
                     }

        Reveal sip -> do
          author sip ∈ dom participants ?! InvalidAuthor (author sip)
          -- TODO: Commited SIP must belong to stable submitted SIPs
          sip ∈ submittedSIPs ?! NoSIPToReveal sip

          sip ∉ revealedSIPs ?! SIPAlreadyRevealed sip
          (Data.calcCommit sip) ∈ (dom commitedSIPs) ?! SIPFailedToBeRevealed sip

          pure st { submittedSIPs = Set.delete sip submittedSIPs
                  , revealedSIPs = Set.insert sip revealedSIPs
                  -- TODO: if stabilization period has passed, then add reveal to stable reveals and remove from reveal
                  -- TODO: A **stable** reveal must open the voting period for this SIP
                  }

        Vote ballot -> do -- error "Define the rules for voting"
            -- voter must be a stakeholder
            (Data.voter ballot) ∈ dom participants ?!
              InvalidVoter (Data.voter ballot)

            -- TODO: SIP must be a stable revealed SIP not just a Revealed SIP
            (Data.votedsip ballot) ∈ revealedSIPs ?!
              VoteNotForRevealedSIP (Data.votedsip ballot)

            -- TODO: Signature of the vote must be verified

            -- TODO:The voting period for this SIP must be open for the vote to be valid

            -- Update State
              -- Add ballot to the state of valid ballots for this SIP
                -- If the voter has voted again, then replace his old vote with the new one
            pure $ st { ballotsForSIP =
                          -- Are there any votes for this SIP yet?
                          if (Data.votedsip ballot) ∈ dom ballotsForSIP
                            then
                              -- insert will overwrite the value part of the Map if the key exists
                              Map.insert
                                (Data.votedsip ballot)
                                (Map.insert
                                    (Data.voter ballot)
                                    ballot
                                    (ballotsForSIP Map.! (Data.votedsip ballot))
                                )
                                ballotsForSIP
                            else
                              Map.insert
                                (Data.votedsip ballot)
                                (Map.fromList [(Data.voter ballot, ballot)])
                                ballotsForSIP
                      }
    ]


instance HashAlgorithm hashAlgo => HasTrace (IDEATION hashAlgo) where

  envGen _traceLength =
    -- TODO: for now we generate a constant set of keys. We need to update the
    -- 'HasTrace' class so that 'trace' can take parameter of an associated
    -- type, so that each STS can decide which parameters are relevant for its
    -- traces.
    pure $! Bimap.fromList
         $  fmap (Core.vKey &&& Core.sKey)
         $  fmap Core.keyPair
         $  fmap Core.Owner $ [0 .. 10]

  -- For now we ignore the predicate failure we might need to provide (if any).
  -- We're interested in valid traces only at the moment.
  sigGen
    participants
    St { submittedSIPs } = do
      owner <- newOwner
      -- generate the new SIP and pass it to generateASubmission "by value"
      -- otherwise you get non-deterministic SIP!
      newsip <- newSIP owner
      case Set.toList submittedSIPs of
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

        newSIP newowner = (SIP)
          <$> newSipHash
          <*> pure newowner
          <*> newSalt
          <*> newSipData
          where
            newSalt = Gen.int (constant 0 100)
            newSipHash = (fmap hash) newSipData -- NullSIPData
            newSipData = (SIPData) <$> (Data.URL <$> Gen.text (constant 1 20) Gen.alpha) <*> (newSIPMetadata)
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

        generateARevelation submittedSIPsList =
          fmap Reveal $ Gen.element submittedSIPsList
