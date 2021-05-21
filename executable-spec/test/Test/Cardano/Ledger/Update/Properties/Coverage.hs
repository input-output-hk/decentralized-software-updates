
module Test.Cardano.Ledger.Update.Properties.Coverage (runTests) where

import           Test.QuickCheck (Property, conjoin, cover, property)
import           Test.Tasty (TestTree)
import           Test.Tasty.QuickCheck (testProperty)

import           SystemUnderTest (SUTAct)
import           Trace (IsTraceFragment, Trace, TraceFragment, invalidActions,
                     scenario)
import           Trace.PropertyTesting (forAllTracesShow)

import           Test.Cardano.Ledger.Update.Events
import           Test.Cardano.Ledger.Update.Interface
import           Test.Cardano.Ledger.Update.Properties.SimpleScenario (Simple,
                     tsUpdateSpecs)
import           Test.Cardano.Ledger.Update.Properties.UpdateSUT (UpdateSUT,
                     getRevealedImpl, getRevealedSIP, getVotedImpl,
                     getVotedSIP)
import           Test.Cardano.Ledger.UpdateSpec (UpdateSpec, getImplId,
                     getImplRevelation, getSIPId, getSIPRevelation)

runTests :: [TestTree]
runTests = [ testProperty "Relevant cases are covered"
           $ forAllTracesShow relevantCasesAreCovered show
           ]
  where
    relevantCasesAreCovered :: Trace UpdateSUT Simple -> Property
    relevantCasesAreCovered trace =
      conjoin $ fmap checkCoverage $ tsUpdateSpecs (scenario trace)
      where
        checkCoverage updateSpec =
          conjoin $ fmap checkEventCoverage $ updateEvents updateSpec trace
          where
            checkEventCoverage (E Unknown fragment)
              = cover 1 (fragment `revealsSIP` updateSpec)
                      "SIP is revealed when SIP is unknown"
              $ cover 1 (fragment `votesSIP` updateSpec)
                      "SIP is voted when SIP is unknown"
              $ cover 1 (fragment `revealsImpl` updateSpec)
                      "Implementation is revealed when SIP is unknown"
              $ cover 1 (fragment `votesImpl` updateSpec)
                      "Implementation is voted when SIP is unknown"
              $ property ()
            checkEventCoverage (E (SIP Submitted) fragment)
              = cover 1 (fragment `revealsSIP` updateSpec)
                        "SIP is revealed when SIP commit is not stable"
              $ cover 1 (fragment `votesSIP` updateSpec)
                        "SIP is voted when SIP commit is not stable"
              $ cover 1 (fragment `revealsImpl` updateSpec)
                        "Implementation is revealed when SIP commit is not stable"
              $ cover 1 (fragment `votesImpl` updateSpec)
                        "Implementation is voted when SIP commit is not stable"
              $ property ()
            checkEventCoverage (E (SIP StablySubmitted) fragment)
              = cover 1 (fragment `votesSIP` updateSpec)
                        "SIP is voted when the SIP is not yet revealed"
              $ cover 1 (fragment `revealsImpl` updateSpec)
                        "Implementation is revealed when the SIP is not yet revealed"
              $ cover 1 (fragment `votesImpl` updateSpec)
                        "Implementation is voted when the SIP is not yet revealed"
              $ property ()
            checkEventCoverage (E (SIP Revealed) fragment)
              = cover 1 (fragment `revealsSIP` updateSpec)
                      "SIP is revealed when the SIP is already revealed"
              $ cover 1 (fragment `votesSIP` updateSpec)
                      "SIP is voted when the SIP revealed"
              $ cover 1 (fragment `revealsImpl` updateSpec)
                      "Implementation is revealed when the SIP is revealed"
              $ cover 1 (fragment `votesImpl` updateSpec)
                      "Implementation is voted when the SIP is revealed"
              $ property ()
            checkEventCoverage (E (SIP StablyRevealed) fragment)
              = cover 1 (fragment `revealsSIP` updateSpec)
                      "SIP is revealed when the SIP is already stably revealed"
              $ cover 1 (fragment `revealsImpl` updateSpec)
                      "Implementation is revealed when the SIP is stably revealed"
              $ cover 1 (fragment `votesImpl` updateSpec)
                      "Implementation is voted when the SIP is stably revealed"
              $ property ()
            -- TODO: add more cases
            checkEventCoverage _ = property ()

revealsSIP :: TraceFragment UpdateSUT -> UpdateSpec -> Bool
revealsSIP = fragmentContains getRevealedSIP getSIPRevelation

votesSIP :: TraceFragment UpdateSUT -> UpdateSpec -> Bool
votesSIP = fragmentContains getVotedSIP getSIPId

revealsImpl :: TraceFragment UpdateSUT -> UpdateSpec -> Bool
revealsImpl = fragmentContains getRevealedImpl getImplRevelation

votesImpl :: TraceFragment UpdateSUT -> UpdateSpec -> Bool
votesImpl = fragmentContains getVotedImpl getImplId


fragmentContains
  :: (Eq a, IsTraceFragment f s)
  =>(SUTAct s -> Maybe a) -> (t -> a) -> f -> t -> Bool
fragmentContains fact fspec frag spec
  = any (\act -> fact act == Just (fspec spec)) $ invalidActions frag
