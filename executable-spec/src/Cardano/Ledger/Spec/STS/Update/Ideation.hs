{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Spec.STS.Update.Ideation where

import           Control.Arrow ((&&&))
import           Control.Monad (mzero)
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.Set as Set
import           Hedgehog ()
import qualified Hedgehog.Gen as Gen

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, TRC (TRC), initialRules, judgmentContext,
                     transitionRules, (?!))
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import           Cardano.Ledger.Spec.STS.Update.Data (Signal (Reveal, Submit),
                     revealedSIPs, submittedSIPs)
import           Cardano.Ledger.Spec.STS.Update.Data (author)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data

import           Ledger.Core (dom, (∈), (∉))
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

  type Signal IDEATION = Data.Signal

  -- We have no failures for now.
  data PredicateFailure IDEATION
    = SIPAlreadySubmitted Data.SIP
    | NoSIPToReveal Data.SIP
    | SIPAlreadyRevealed Data.SIP
    | InvalidAuthor Core.VKey
    deriving (Eq, Show)

  initialRules = [ pure $! mempty ]

  transitionRules = [
    do
      TRC ( participants
          , st@Data.State { submittedSIPs, revealedSIPs }
          , sig
          ) <- judgmentContext
      case sig of
        Submit sip -> do
          author sip ∈ dom participants ?! InvalidAuthor (author sip)
          sip ∉ submittedSIPs ?! SIPAlreadySubmitted sip
          pure $! st { submittedSIPs = Set.insert sip submittedSIPs }
        Reveal sip -> do
          author sip ∈ dom participants ?! InvalidAuthor (author sip)
          sip ∈ submittedSIPs ?! NoSIPToReveal sip
          sip ∉ revealedSIPs ?! SIPAlreadyRevealed sip
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
    Data.State { submittedSIPs, revealedSIPs }
    =
    Gen.frequency [ (1, generateASubmission)
                  , (1, generateARevelation)
                  ]
    where
      -- Generate a submission taking a participant that hasn't submitted a proposal yet
      generateASubmission
        = fmap (Submit . (nextId `Data.SIP`))
        $ Gen.element
        $ Set.toList
        $ dom participants
        where
          nextId = maximum $ 0 : fmap (1+) ids
          ids =  Set.toList (Set.map Data.id submittedSIPs)
              ++ Set.toList (Set.map Data.id revealedSIPs)

      generateARevelation =
        case Set.toList submittedSIPs of
          [] -> mzero -- We cannot generate a revelation, since there are no
                      -- (SIP) commitments yet, so we retry (in this case
                      -- eventually we'll generate a submission).
          xs -> fmap Reveal $ Gen.element xs
