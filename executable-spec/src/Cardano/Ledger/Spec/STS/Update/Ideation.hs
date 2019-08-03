{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Ledger.Spec.STS.Update.Ideation where

import           Control.Arrow ((&&&))
import           Control.Monad (mzero)
import           Data.Bimap (Bimap, (!))
import qualified Data.Bimap as Bimap
import qualified Data.Set as Set
import           Hedgehog ()
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Range (constant)

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, TRC (TRC), initialRules, judgmentContext,
                     transitionRules, (?!))
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import           Cardano.Ledger.Spec.STS.Update.Data (Signal (Reveal, Submit),
                      SIP(..), SIPData(..), revealedSIPs, submittedSIPs, commitedSIPs)
import           Cardano.Ledger.Spec.STS.Update.Data (author)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data

import           Ledger.Core (dom, (∈), (∉), hash)
import qualified Ledger.Core as Core
import qualified Data.Map.Strict as Map
-- import qualified Debug.Trace as Debug

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

  type Signal IDEATION = Data.Signal

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
          --
          -- debug
          -- let !dummy3 = Debug.trace ("calcCommit: " ++ show (Data.calcCommit sip) ++ " " ++ (show $ Data.salt sip)) True
          --     !dummy4 = Debug.trace ("LookUpCommit: " ++ show (Map.lookup (Data.calcCommit sip) commitedSIPs)) True

          -- case Map.lookup (Data.calcCommit sip) commitedSIPs of
          --   Nothing -> False
          --   Just _ -> True
          --   ?! SIPFailedToBeRevealed sip
          (Data.calcCommit sip) ∈ (dom commitedSIPs) ?! SIPFailedToBeRevealed sip
          pure st { submittedSIPs = Set.delete sip submittedSIPs
                  , revealedSIPs = Set.insert sip revealedSIPs
                  }
    ]

instance HasTrace IDEATION where

  envGen _traceLength =
    -- TODO: for now we generate a constant set of keys. We need to update the
    -- 'HasTrace' so that 'trace' can take parameter of an associated type, so
    -- that each STS can decide which parameters are relevant for its traces.
    pure $! Bimap.fromList
         $  fmap (Core.vKey &&& Core.sKey)
         $  fmap Core.keyPair
         $  fmap Core.Owner $ [0 .. 10]

  -- For now we ignore the predicate failure we might need to provide (if any).
  -- We're interested in valid traces only at the moment.
  sigGen
    _maybePredicateFailure
    participants
    Data.State { submittedSIPs } = do
      owner <- newOwner
      -- generate the new SIP and pass it to generateASubmission "by value"
      -- otherwise you get non-deterministic SIP!
      newsip <- newSIP owner
      Gen.frequency [ (1, generateASubmission newsip owner)
                    , (1, generateARevelation)]
      where
        newOwner =
          Gen.element
          $ Set.toList
          $ dom participants

        newSIP newowner = (SIP)
          -- <$> pure nextId
          -- <*> newSipHash
          <$> newSipHash
          <*> pure newowner
          <*> newSalt
          <*> newSipData
          where
            -- nextId = maximum $ (Data.UpId 0) : fmap ((Data.UpId 1)+) ids
            -- ids =  Set.toList (Set.map Data.id submittedSIPs)
            --     ++ Set.toList (Set.map Data.id revealedSIPs)
            newSalt = Gen.int (constant 0 100)
            newSipHash = (fmap hash) newSipData -- NullSIPData
            newSipData = (SIPData) <$> (Gen.text (constant 1 50) Gen.alpha) <*> (newSIPMetadata)
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
          -- debug
          --let
          --   !dummy1 = Debug.trace ("owner: " ++ show owner) True
          --   !dummy2 = Debug.trace ("newCommit: " ++ show ncommit) True
          --   !dummy5 = Debug.trace ("newSIP: " ++ show nsip) True
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

        generateARevelation =
          case Set.toList submittedSIPs of
            [] -> mzero -- We cannot generate a revelation, since there are no
                        -- (SIP) commitments yet, so we retry (in this case
                        -- eventually we'll generate a submission).
            xs -> fmap Reveal $ Gen.element xs
