{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.SM.Vote where

import           Prelude hiding (subtract)

import           Control.Applicative ((<|>))
import           Control.Arrow (second)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           GHC.Exts (IsList)
import           Test.QuickCheck

import           Control.State.DataAutomata
import           Control.State.DataAutomata.Expr
import           Control.State.DataAutomata.Interpreter.Memory hiding (Error)
import           Control.State.DataAutomata.Interpreter.Run

import           Cardano.Ledger.Spec.SM.Ideation

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data

newtype Participant = Participant Word
  deriving (Eq, Show, Ord)

data Vote
  = Vote
    { vk :: Participant -- I don't know if we care about keys here. We could rename this to "who".
    , decision :: Decision
    }
    deriving (Eq, Show)

data Decision = For | Against | Abstain
  deriving (Eq, Show)

data Verdict = NoVerdict | NoQuorum | Reject | Approve
  deriving (Eq, Show)

instance ToExpr Verdict Verdict where
  toExpr = Const

data StakeUpdate
  = StakeUpdate
    { fromVk :: Participant
    , toVk :: Participant
    , percentToTransfer :: Double -- TODO: This should be a 'Percentage' newtype
                                  -- with a smart constructor that ensures the
                                  -- value is in the range [0, 1].
    }
    deriving (Show, Eq)

newtype StakeDistribution = StakeDistribution (Map Participant Data.Stake)
  deriving (Eq, Show)

totalStake :: StakeDistribution -> Data.Stake
totalStake (StakeDistribution stakeMap) = sum $ Map.elems stakeMap

updateStake :: StakeDistribution -> StakeUpdate -> StakeDistribution
updateStake stake@(StakeDistribution stakeMap) (StakeUpdate { fromVk, toVk, percentToTransfer }) =
  fromMaybe stake $
  do
  stakeFrom <- Map.lookup fromVk stakeMap
  stakeTo <- Map.lookup toVk stakeMap
  let stakeFrom'       = stakeFrom - transferedAmount
      stakeTo'         = stakeTo   + transferedAmount
      transferedAmount = fromIntegral
                       $ (round (fromIntegral stakeFrom * percentToTransfer) :: Int)
  pure $! StakeDistribution
       $! Map.insert fromVk stakeFrom'
       $! Map.insert toVk   stakeTo'  stakeMap

newtype SIPBallot = SIPBallot (Map Participant Decision)
  deriving (Eq, Show, IsList)

updateBallot :: SIPBallot -> Vote -> SIPBallot
updateBallot (SIPBallot votesMap) (Vote { vk, decision }) =
  SIPBallot votesMap'
  where
    votesMap' = Map.insert vk decision votesMap

-- | Model of the vote count.
voteTally :: Word -> DataAutomaton
voteTally autId =
  DataAutomaton
  { start = "Active"
  , transitions =
    [ "Active" .-- ( "vote" .@ autId #? vote
                     , votes := Fapply2 "updateBallot" (VarE votes) (VarE vote)
                     ) .--> "Active"
    , "Active" .-- ( "stakeUpdate" #? update
                   , stake := Fapply2 "updateStake" (VarE stake) (VarE update)
                   )
               .--> "Active"
    , "Active" .-- ("tally" .@! autId)
               .--> "Tallying"
    , "Tallying" .-- ( Approve .== (Fapply3 "tally" (VarE votes) (VarE stake) (VarE tauV))
                     , "approve" .@! autId
                     )
                 .--> "Approved"
    , "Tallying" .-- ( Reject .== (Fapply3 "tally" (VarE votes) (VarE stake) (VarE tauV))
                     , "reject" .@! autId
                     )
                 .--> "Rejected"
    , "Tallying" .-- ( NoQuorum .== (Fapply3 "tally" (VarE votes) (VarE stake) (VarE tauV))
                     , "abstain" .@! autId
                     )
                 .--> "Abstained"
    , "Tallying" .-- ( NoVerdict .== (Fapply3 "tally" (VarE votes) (VarE stake) (VarE tauV))
                     , "noverdict" .@! autId
                     )
                 .--> "Undecided"
    ]
  }
  where
    vote :: Var Vote
    vote = "vote"

    votes :: Var SIPBallot
    votes = "votes"

    stake :: Var StakeDistribution
    stake = "stake"

    update :: Var StakeUpdate
    update = "update"

    tauV :: Var Word
    tauV = "tauV"

initVoteTallyMem :: Word -> StakeDistribution -> Memory
initVoteTallyMem aTauV aStakeDist
  = [ ("tauV", Cell aTauV)
    , ("stake", Cell aStakeDist)
      -- The memory entries above ^ should be generated!
    , ("votes", Cell ([] :: SIPBallot))
    , ("vk", Cell (Function vk))
    , ("tally", Cell (Function tally))
    , ("updateStake", Cell (Function updateStake))
    , ("updateBallot", Cell (Function updateBallot))
    ]

tally :: SIPBallot -> StakeDistribution -> Word -> Verdict
tally (SIPBallot votesMap) stake@(StakeDistribution stakeMap) tauV
  =   fromMaybe  NoVerdict
  $   when (tauV <= stakePct For) Approve
  <|> when (tauV <= stakePct Against) Reject
  <|> when (tauV <= stakePct Abstain) NoQuorum

  where
    when cond verdict = if cond then Just verdict else Nothing

    stakePct :: Decision -> Word
    stakePct how = round $ 100 * stakeThatVoted how / fromIntegral (totalStake stake)

    stakeThatVoted :: Decision -> Double
    stakeThatVoted how
      = fromIntegral
      $ sum
      $ Map.elems
      $ Map.intersection stakeMap
      $ Map.filter (== how) votesMap

--------------------------------------------------------------------------------
-- Model the vote activation (stability aspect)
--------------------------------------------------------------------------------

activation :: Word -> DataAutomaton
activation autId =
  DataAutomaton
  { start = "Idle"
  , transitions =
    [ "Idle"     .-- ("tick" #? s, cslot .= s )                                        .--> "Idle"
    , "Idle"     .-- ("reveal" .@! autId, rslot .= cslot)                              .--> "Revealed"
    , "Revealed" .-- (cslot .< rslot .+ _2 .* k, "tick" #? s, cslot .= s )             .--> "Revealed"
    , "Revealed" .-- (rslot .+ _2 .* k .<= cslot, "active" .@! autId, aslot .= cslot ) .--> "Active"
    , "Active"   .-- ("vote" .@ autId #? _vote)  .--> "Active"
    , "Active"   .-- (s .< aslot .+ vpd , "tick" #? s, cslot .= s )                    .--> "Active"
    , "Active"   .-- (aslot .+ vpd .<= s, "tick" #? s, vpeslot .= s )                  .--> "EndingVP"
    , "EndingVP" .-- ("vpend" .@! autId)                                               .--> "VPEnded"
    , "VPEnded"  .-- (cslot .< vpeslot .+ _2 .* k, "tick" #? s, cslot .= s )           .--> "VPEnded"
    , "VPEnded"  .-- (vpeslot .+ _2 .* k .<= cslot, "tally" .@! autId )                .--> "Tallying"
    -- Note that it is important not to block the tick events on a terminal
    -- state. Otherwise the time will stop in the "Tallying" state.
    , "Tallying" .-- ("tick" #? s )                                                    .--> "Tallying"
    ]
  }
  where
    s :: Var Word
    s = "s"

    _2 :: Word
    _2 = 2

    k :: Var Word
    k = "k"

    -- Voting period duration
    vpd :: Var Word
    vpd = "vpd"

    -- Current slot
    cslot :: Var Word
    cslot = "cslot"

    -- Revelation slot
    rslot :: Var Word
    rslot = "rslot"

    -- Activation slot
    aslot :: Var Word
    aslot = "aslot"

    -- Slot at which the voting period ended
    vpeslot :: Var Word
    vpeslot = "vpeslot"

    _vote :: Var Vote
    _vote = "_vote"

initActivationMem :: Word -> Word -> Memory -- TODO: use newtype wrappers
initActivationMem k vpd
  = [ ("k", Cell k) -- TODO: generate
    , ("vpd", Cell vpd) -- TODO: generate
    , ("cslot", Cell (0 :: Word))
    , ("rslot", Cell (0 :: Word))
    , ("aslot", Cell (0 :: Word))
    , ("vpeslot", Cell (0 :: Word))
    ]

--------------------------------------------------------------------------------
--  Majority's preference
--------------------------------------------------------------------------------

-- | Model of the majority's preference.
majority :: Word -> DataAutomaton
majority i =
  DataAutomaton
  { start = "Idle"
  , transitions =
    [ "Idle"                 .-- ("majorityAgrees" #! ()                     ) .--> "VotingMainlyFor"
    , "VotingMainlyFor"      .-- ("createVoteFor" .@ i #? vote, nvote .= vote) .--> "IssuingVoteMainlyFor"
    , "IssuingVoteMainlyFor" .-- ("vote" .@ i #!! VarE nvote                 ) .--> "VotingMainlyFor"


    , "Idle"                     .-- ("majorityRejects" #! ()                        ) .--> "VotingMainlyAgainst"
    , "VotingMainlyAgainst"      .-- ("createVoteAgainst" .@ i #? vote, nvote .= vote) .--> "IssuingVoteMainlyAgainst"
    , "IssuingVoteMainlyAgainst" .-- ("vote" .@ i #!! VarE nvote                     ) .--> "VotingMainlyAgainst"
    ]
  }
  where
    vote :: Var Vote
    vote = "vote"

    -- Next vote
    nvote :: Var Vote
    nvote = "nvote"


initMajorityMem :: Memory
initMajorityMem = [ ("nvote", Cell (Vote (Participant 0) For))] -- TODO: we need this to be able to show the uninitialized memory

genVoteMainlyFor :: [Participant] -> Gen Vote
genVoteMainlyFor participants = genVote' participants freqs
  where
    -- TODO: here we could use @tauV@ to determine the frequency.
    freqs =  [ (90, For)
             , (5, Against)
             , (5, Abstain)
             ]

genVoteMainlyAgainst :: [Participant] -> Gen Vote
genVoteMainlyAgainst participants = genVote' participants freqs
  where
    -- TODO: here we could use @tauV@ to determine the frequency.
    freqs =  [ (5, For)
             , (90, Against)
             , (5, Abstain)
             ]

genVote' :: [Participant] -> [(Int, Decision)] -> Gen Vote
genVote' participants freqs = do
  who <- elements participants
  aDecision <- frequency $ fmap (second pure) freqs
  pure $ Vote { vk = who
              , decision = aDecision
              }

--------------------------------------------------------------------------------
-- TODO: Model the revoting aspect
--------------------------------------------------------------------------------

-- | Complete model of a SIP voting process
--
-- TODO: we still need to model revote
votingModel :: Word -> Automata
votingModel sipId
  = (   Sync ["tick"                                   ] (Single ticker)
    :|| Sync ["tick", "tally" .@ sipId, "vote" .@ sipId] (Single $ activation sipId)
    :|| Sync [        "tally" .@ sipId, "vote" .@ sipId] (Single $ voteTally sipId)
    :|| Sync [                          "vote" .@ sipId] (Single $ majority sipId)
    )

votesGen :: [Participant] -> Word -> Map ActionName (Gen Cell)
votesGen participants sipId =
  [ ("createVoteFor" .@ sipId, Cell <$> genVoteMainlyFor participants)
  , ("createVoteAgainst" .@ sipId, Cell <$> genVoteMainlyAgainst participants)
  ]

initialVotingModelMem :: Word -> StakeDistribution -> Word -> Word -> Word -> LTree Memory
initialVotingModelMem aTauV aStakeDist k vpd currentSlot
  =   Leaf (initTickerMem currentSlot)
  :++ Leaf (initActivationMem k vpd)
  :++ Leaf (initVoteTallyMem aTauV aStakeDist)
  :++ Leaf initMajorityMem

votingModelGenerators :: [Participant] -> Word -> Map ActionName (Gen Cell)
votingModelGenerators participants sipId
  = tickerActsGen <> votesGen participants sipId
