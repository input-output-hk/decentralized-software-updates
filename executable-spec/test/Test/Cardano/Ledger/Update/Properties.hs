{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Update.Properties
  (runTests)
where

import           Control.Arrow (second)
import           Test.Tasty (TestTree)
import           Test.Tasty.QuickCheck (testProperty)

import           Test.QuickCheck (expectFailure, withMaxSuccess)

import           Cardano.Ledger.Assert (prettyShow)

import           Cardano.Ledger.Update.ProposalsState
                     (Decision (Approved, Expired, Rejected, WithNoQuorum))

import qualified Cardano.Ledger.Update as Update

import           Trace
import           Trace.PropertyTesting

import           Test.Cardano.Ledger.UpdateSpec

import           Test.Cardano.Ledger.Update.Properties.Liveness
import           Test.Cardano.Ledger.Update.Properties.SimpleScenario
import qualified Test.Cardano.Ledger.Update.Properties.StateChangeValidity as StateChangeValidity
import           Test.Cardano.Ledger.Update.Properties.UpdateSUT

import qualified Test.Cardano.Ledger.Update.Properties.CBORSerialisation as CBORSerialisation

runTests :: [TestTree]
runTests = [
           ---------------------------------------------------------------------
           -- Ideation phase liveness properties
           ---------------------------------------------------------------------
            testProperty "SIP's are expired"
             $ expectFailure
             $ forAllTracesShow (sipsAreNot Expired) (const "")
             -- Use:
             --
             -- > showActionsAndStateOfUpdateSpec
             --
             -- instead of @const ""@ to show the trace.
           , testProperty "SIP's are rejected"
             $ expectFailure
             $ forAllTracesShow (sipsAreNot Rejected) (const "")
           , testProperty "SIP's get no-quorum"
             $ expectFailure
             $ forAllTracesShow (sipsAreNot WithNoQuorum) (const "")
           , testProperty "SIP's are approved"
             -- NOTE: here we might have an approved SIP in the trace without
             -- passing through the @Approved@ state. This could happen if an
             -- implementation for the SIP is submitted before the SIP is
             -- approved. In that case `stateOf` will return @Implementation
             -- Submitted@ or @Implementation StablySubmitted@.
             $ expectFailure
             $ forAllTracesShow (sipsAreNot Approved) (const "")
           ---------------------------------------------------------------------
           -- Implementation phase liveness properties
           ---------------------------------------------------------------------
           , testProperty "Implementations are expired"
             $ expectFailure
             $ forAllTracesShow (implsAreNot Expired) (const "")
           , testProperty "Implementations are rejected"
             $ expectFailure
             $ forAllTracesShow (implsAreNot Rejected) (const "")
           , testProperty "Implementations get no-quorum"
             $ expectFailure
             $ forAllTracesShow (implsAreNot WithNoQuorum) (const "")
           -- We do not test that implementations are approved since that is
           -- implicitly tested when we test that updates are activated.
           ---------------------------------------------------------------------
           -- Activation phase liveness properties
           ---------------------------------------------------------------------
           , testProperty "Implementations are activated"
             $ expectFailure
             $ withMaxSuccess 10000
             $ forAllTracesShow updatesAreNotActivated (const "")
           , testProperty "Implementations are queued"
             $ expectFailure
             $ forAllTracesShow updatesAreNotQueued (const "")
           , testProperty "Updates are discarded due to being expired"
             $ expectFailure
             $ withMaxSuccess 10000
             $ forAllTracesShow
                 (updatesAreNotDiscardedDueToBeing Update.Expired)
                 (const "")
           , testProperty "Updates are discarded due to being unsupported"
             $ expectFailure
             $ withMaxSuccess 10000
             $ forAllTracesShow
                 (updatesAreNotDiscardedDueToBeing Update.Unsupported)
                 (const "")
           ---------------------------------------------------------------------
           -- Update system safety properties
           ---------------------------------------------------------------------
           , testProperty "Changes in the state of update proposals are valid."
             $ withMaxSuccess 2000
             $ forAllTracesShow
                 StateChangeValidity.prop_updateEventTransitionsAreValid
                 showActionsAndStateOfUpdateSpec
           ---------------------------------------------------------------------
           -- Coverage testing
           ---------------------------------------------------------------------
           -- TODO: test that the cases below are generated:
           -- - SIP's and implementations got approved in the second voting period.
           -- - SIP's and implementations got approved in the last voting period.
           -- - protocol updates got canceled:
           --   - before approval
           --   - when the update was waiting in the queue
           --   - when the update was a candidate
           -- - protocol updated is adopted after being endorsed across several
           --   epochs.
           ---------------------------------------------------------------------
           -- CBOR serialisation tests
           ---------------------------------------------------------------------
           , testProperty "Data is correctly serialised to CBOR"
             $ withMaxSuccess 5000
             $ CBORSerialisation.statesAreCorrectlyEncoded
           ]

--------------------------------------------------------------------------------
-- Custom trace rendering
--------------------------------------------------------------------------------

-- | Show the actions and the state of the update specs in the test setup.
showActionsAndStateOfUpdateSpec :: Trace UpdateSUT Simple -> String
showActionsAndStateOfUpdateSpec trace
  =  "Scenario: \n"
  ++ replicate 80 '-' ++ "\n"
  ++ prettyShow (scenario trace)
  ++ "\nInitial state: \n"
  ++ replicate 80 '-' ++ "\n"
  ++ prettyShow (stateOfSpecsAt (initialState trace))
  ++ "\nEvents: \n"
  ++ replicate 80 '-' ++ "\n"
  ++ prettyShow (fmap (second stateOfSpecsAt) (validTransitions trace))
  where
    updateSpecs = tsUpdateSpecs $ scenario trace
    stateOfSpecsAt st = fmap (`stateOf` (unUpdateSt st)) updateSpecs
