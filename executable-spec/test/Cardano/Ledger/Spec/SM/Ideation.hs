{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | TODO: this module should be named 'CommitReveal' or something like that.
module Cardano.Ledger.Spec.SM.Ideation where

import           Data.Map.Strict (Map)
import           Test.QuickCheck

import           Control.State.DataAutomata
import           Control.State.DataAutomata.Expr
import           Control.State.DataAutomata.Interpreter.Memory hiding (Error)
import           Control.State.DataAutomata.Interpreter.Run


-- | Model of slot ticks.
ticker :: DataAutomaton
ticker =
  DataAutomaton
  { start = "Ticking"
  , transitions =
    [ "Ticking" .-- ( (0 :: Word) .< i
                    , "setInc" #? i
                    , inc := VarE i
                    )
                                              .--> "Ticking"
    , "Ticking" .-- ( "tick" #!! cslot .+ inc
                     , cslot := cslot .+ inc
                     )
                                              .--> "Ticking"
    ]
  }
  where
    i :: Var Word
    i = "i"

    inc :: Var Word
    inc = "inc"

    cslot :: Var Word
    cslot = "cslot"

tickerActsGen :: Map ActionName (Gen Cell)
-- TODO: we tick in increments of 1 in the interface, so setting the increment
-- to make a jump of more than 1 will break the model. These conformance tests
-- will be soon replaced by better ones which do not duplicate the logic of the
-- STS.
tickerActsGen = [ ("setInc", Cell <$> (elements [1 .. (1 :: Word)])) ]

initTickerMem :: Word -> Memory
initTickerMem currentSlot =
  [ ("inc", Cell (0 :: Word))
  , ("cslot", Cell currentSlot)
  ]

-- | Commit-reveal model.
commitReveal :: Word -> DataAutomaton
commitReveal autId  =
  DataAutomaton
  { start = "Idle"
  , transitions =
    [ "Idle"      .-- ( "tick" #? s, cslot .= s )                 .--> "Idle"
    , "Idle"      .-- ( "submit" .@ autId #! (), sslot .= cslot ) .--> "Submitted"
    , "Submitted" .-- ( "tick" #? s, cslot .= s )                 .--> "Submitted"
    , "Submitted" .-- ( sslot .+ _2 .* k .<= cslot
                      , "reveal" .@ autId #! ()
                      , rslot .= cslot
                      )                                           .--> "Revealed"
     -- We need to accept ticks to make sure other automata that synchronizes on
     -- this action can proceed.
    , "Revealed" .-- ( "tick" #? s )                              .--> "Revealed"
    ]
  }
  where
    s :: Var Word
    s = "s"

    k :: Var Word
    k = "k"

    _2 :: Word
    _2 = 2

    cslot :: Var Word
    cslot = "cslot"

    sslot :: Var Word
    sslot = "sslot"

    rslot :: Var Word
    rslot = "rslot"

initCommitRevealMem :: Word -> Word -> Memory
initCommitRevealMem k currentSlot =
  -- Mmmm, maybe it'd be more ergonomic if we didn't have to declare these beforehand.
  [ ("k", Cell k)
  , ("cslot", Cell currentSlot)
  , ("sslot", Cell (0 :: Word))
  , ("rslot", Cell (0 :: Word))
  ]

--------------------------------------------------------------------------------
-- Commit-reveal model
--------------------------------------------------------------------------------

-- | Commit/reveal full model.
--
-- TODO: after placing this function in a @CommitReveal@ module we can rename
-- it.
crFullModel :: Word -> Automata
crFullModel sipId
  =   Sync ["tick"] (Single ticker)
  :|| Sync ["tick"] (Single $ commitReveal sipId)

crFullModelInitMem :: Word -> Word -> LTree Memory
crFullModelInitMem k currentSlot
  =   Leaf (initTickerMem currentSlot)
  :++ Leaf (initCommitRevealMem k currentSlot)
