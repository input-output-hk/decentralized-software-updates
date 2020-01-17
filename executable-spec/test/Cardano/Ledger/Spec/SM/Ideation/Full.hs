{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Ledger.Spec.SM.Ideation.Full where

import           Data.Map.Strict (Map)
import           Test.QuickCheck

import           Control.State.DataAutomata
import           Control.State.DataAutomata.Expr
import           Control.State.DataAutomata.Interpreter.Memory
import           Control.State.DataAutomata.Interpreter.Run

import           Cardano.Ledger.Spec.SM.Ideation
import           Cardano.Ledger.Spec.SM.Vote


-- | Complete ideation model, for a given SIP.
--
fullModel :: Word -> Automata
fullModel aSipId
  =   Sync [ "tick"
           , "reveal" .@ aSipId
           ]
           (Single $ commitReveal aSipId)
  :|| Sync ["tick"
           , "active" .@ aSipId
           , "vote" .@ aSipId
           , "vpend" .@ aSipId
           ] (Single $ ensureVotesPerSlot aSipId)
  :|| Sync [ "tick"
           , "reveal" .@ aSipId
           , "active" .@ aSipId
           , "vote".@ aSipId
           , "vpend" .@ aSipId
           , "tally" .@ aSipId
           ]
           (Single $ activation aSipId)
  :|| Sync [ "tally".@ aSipId
           , "vote".@ aSipId
           ]
           (Single $ voteTally aSipId)
  :|| Sync ["vote".@ aSipId
           ]
           (Single $ majority aSipId)

-- | This model is used to allow a certain number of votes per slot tick.
ensureVotesPerSlot :: Word -> DataAutomaton
ensureVotesPerSlot aSipId =
  DataAutomaton
  { start = "Idle"
  , transitions =
    [ "Idle"   .-- ("tick" #? s)                                     .--> "Idle"
    , "Idle"   .-- ("active" .@! aSipId, c .= (0 :: Word))           .--> "Active"
    , "Active" .-- ("vote" .@ aSipId #? vote, c .= c .+ (1 :: Word)) .--> "Active"
    , "Active" .-- ( vpp .< c, "tick" #? s)                          .--> "Active"
    , "Active" .-- ("vpend" .@! aSipId)                              .--> "Idle"

    ]
  }
  where
    s :: Var Word
    s = "s"

    c :: Var Word
    c = "c"

    vote :: Var Vote
    vote = "vote"

    -- | Votes per period
    vpp :: Var Word
    vpp = "vpp"

ensureVotesPerSlotInitMem :: Word -> Memory
ensureVotesPerSlotInitMem vpp =
  [ ("c", Cell (0 :: Word))
  , ("vpp", Cell vpp)
  ]

fullModelInitMem :: Word -> StakeDistribution -> Word -> Word -> Word -> LTree Memory
fullModelInitMem aTauV aStakeDist k vpd currentSlot
  =   Leaf (initCommitRevealMem k currentSlot)
  -- Note that after a certain point, increasing the votes per-period means
  -- that we will use the trace length budget for votes only, so we won't reach
  -- the tally phase.
  :++ Leaf (ensureVotesPerSlotInitMem 2)
  :++ Leaf (initActivationMem k vpd)
  :++ Leaf (initVoteTallyMem aTauV aStakeDist)
  :++ Leaf initMajorityMem

fullModelGens :: [Participant] -> Word -> Map ActionName (Gen Cell)
fullModelGens participants aSipId = votesGen participants aSipId
