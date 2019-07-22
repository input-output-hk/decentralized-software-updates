{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Spec.STS.Update.Ideation where

import qualified Data.Set as Set

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, TRC (TRC), initialRules, judgmentContext,
                     transitionRules, (?!))

import           Cardano.Ledger.Spec.STS.Update.Data (Signal (Reveal, Submit),
                     revealedSIPs, submittedSIPs)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data

import           Ledger.Core ((∈), (∉))

--------------------------------------------------------------------------------
-- Updates ideation phase
--------------------------------------------------------------------------------

-- | Ideation phase of system updates
data IDEATION

instance STS IDEATION where

  -- We have no environment for the time being.
  type Environment IDEATION = ()

  type State IDEATION = Data.State

  type Signal IDEATION = Data.Signal

  -- We have no failures for now.
  data PredicateFailure IDEATION
    = SIPAlreadySubmitted Data.SIP
    | NoSIPToReveal Data.SIP
    | SIPAlreadyRevealed Data.SIP
    deriving (Eq, Show)

  initialRules = [ pure $! mempty ]

  transitionRules = [
    do
      TRC ( ()
          , st@Data.State { submittedSIPs, revealedSIPs }
          , sig
          ) <- judgmentContext
      case sig of
        Submit sip -> do
          sip ∉ submittedSIPs ?! SIPAlreadySubmitted sip
          pure $! st { submittedSIPs = Set.insert sip submittedSIPs }
        Reveal sip -> do
          sip ∈ submittedSIPs ?! NoSIPToReveal sip
          sip ∉ revealedSIPs ?! SIPAlreadyRevealed sip
          pure st {revealedSIPs = Set.insert sip revealedSIPs }
    ]
