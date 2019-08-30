{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Update.Ideation where

import           Control.Arrow ((&&&))
import           Data.Bimap (Bimap, (!))
import qualified Data.Bimap as Bimap
import qualified Data.Set as Set
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Range (constant)

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, TRC (TRC), initialRules, judgmentContext,
                     transitionRules, (?!))
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import           Cardano.Ledger.Spec.STS.Update.Data
                     (IdeationPayload (Reveal, Submit, Vote), SIP (SIP),
                     SIPData (SIPData), commitedSIPs, revealedSIPs,
                     submittedSIPs)
import           Cardano.Ledger.Spec.STS.Update.Data (author)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data

import qualified Data.Map.Strict as Map
import           Ledger.Core (dom, hash, (∈), (∉))
import qualified Ledger.Core as Core

--------------------------------------------------------------------------------
-- Updates ideation phase
--------------------------------------------------------------------------------


-- | Ideation phase of system updates
data IDEATION

instance STS IDEATION where

  -- | The environment is the set of participants, identified by their signing
  -- and verifying keys.
  --
  -- There is a one-to-one correspondence the signing and verifying keys, hence
  -- the use of 'Bimap'
  type Environment IDEATION = Bimap Core.VKey Core.SKey

  type State IDEATION = Data.State

  type Signal IDEATION = IdeationPayload

  -- We have no failures for now.
  data PredicateFailure IDEATION
    = SIPAlreadySubmitted Data.SIP
    | NoSIPToReveal Data.SIP
    | SIPAlreadyRevealed Data.SIP
    | InvalidAuthor Core.VKey
    | SIPFailedToBeRevealed Data.SIP
    deriving (Eq, Show)

  initialRules = [ pure $! mempty ]

  transitionRules = [
    do
      TRC ( participants
          , st@Data.State { commitedSIPs, submittedSIPs, revealedSIPs }
          , sig
          ) <- judgmentContext
      case sig of
        Submit sipc sip -> do
          author sip ∈ dom participants ?! InvalidAuthor (author sip)
          sip ∉ submittedSIPs ?! SIPAlreadySubmitted sip
          pure $! st { commitedSIPs = Map.insert (Data.commit sipc) (sipc) commitedSIPs
                     , submittedSIPs = Set.insert sip submittedSIPs
                     }
        Reveal sip -> do
          author sip ∈ dom participants ?! InvalidAuthor (author sip)
          sip ∈ submittedSIPs ?! NoSIPToReveal sip
          sip ∉ revealedSIPs ?! SIPAlreadyRevealed sip
          (Data.calcCommit sip) ∈ (dom commitedSIPs) ?! SIPFailedToBeRevealed sip
          pure st { submittedSIPs = Set.delete sip submittedSIPs
                  , revealedSIPs = Set.insert sip revealedSIPs
                  }
        Vote _ -> error "Define the rules for voting"
    ]


instance HasTrace IDEATION where

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
    Data.State { submittedSIPs } = do
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
