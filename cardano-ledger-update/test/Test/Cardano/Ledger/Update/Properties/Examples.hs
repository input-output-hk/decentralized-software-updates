{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Module with the necessary imports and helper functions to debug examples
-- and properties of the update mechanism.
--
-- Counterexamples returned by QuickCheck can be pasted here so that they can be
-- explored in the REPL.
module Test.Cardano.Ledger.Update.Properties.Examples where

import           GHC.Exts (fromList)
import           Test.QuickCheck

import           Cardano.Slotting.Block
import           Cardano.Slotting.Slot

import           Cardano.Ledger.Update
import           Cardano.Ledger.Update.Env.HasVotingPeriodsCap
import           Cardano.Ledger.Update.Proposal
import           Cardano.Ledger.Update.ProposalsState

import           Test.Cardano.Ledger.Update.Data
import           Test.Cardano.Ledger.Update.Data.MockProposal
import           Test.Cardano.Ledger.Update.Properties.SimpleScenario
import           Test.Cardano.Ledger.UpdateSpec

import           Trace.Generation

import           Test.Cardano.Ledger.Update.Properties.Liveness
import           Test.Cardano.Ledger.Update.Properties.UpdateSUT


isApproved :: Scenario Simple -> Bool
isApproved ts = not $ sipsAreNot Approved $ elaborateTrace ts

--------------------------------------------------------------------------------
-- Delete any examples below this line
--------------------------------------------------------------------------------
