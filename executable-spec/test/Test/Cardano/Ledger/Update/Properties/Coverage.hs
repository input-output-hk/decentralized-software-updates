
module Test.Cardano.Ledger.Update.Properties.Coverage (runTests) where

import           Test.QuickCheck (Property, checkCoverage, conjoin, cover,
                     property)
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
                     getRevealedImpl, getRevealedSIP, getSubmittedImpl,
                     getSubmittedSIP, getVotedImpl, getVotedSIP)
import           Test.Cardano.Ledger.UpdateSpec (UpdateSpec, getImplId,
                     getImplRevelation, getImplSubmission, getSIPId,
                     getSIPRevelation, getSIPSubmission)

runTests :: [TestTree]
runTests = [ testProperty "Relevant cases are covered"
           $ checkCoverage
           $ forAllTracesShow relevantCasesAreCovered show
           ]
  where
    relevantCasesAreCovered :: Trace UpdateSUT Simple -> Property
    relevantCasesAreCovered trace =
      conjoin $ fmap checkTraceCoverage $ tsUpdateSpecs (scenario trace)
      where
        checkTraceCoverage updateSpec =
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
            checkEventCoverage (E (SIP (Is _)) fragment)
              = cover 1 (fragment `submitsSIP` updateSpec)
                        "SIP is submitted when a verdict on it was reached"
              $ cover 1 (fragment `revealsSIP` updateSpec)
                        "SIP is revealed when a verdict on it was reached"
              $ cover 1 (fragment `votesSIP` updateSpec)
                        "SIP is voted when a verdict on it was reached"
              $ cover 1 (fragment `revealsImpl` updateSpec)
                        "Implementation is revealed when a verdict on its SIP was reached"
              $ cover 1 (fragment `votesImpl` updateSpec)
                        "Implementation is voted when a verdict on its SIP was reached"
              $ property ()
            checkEventCoverage (E (SIP (IsStably _)) fragment)
              = cover 1 (fragment `submitsSIP` updateSpec)
                        "SIP is submitted when a verdict on it was stably reached"
              $ cover 1 (fragment `revealsSIP` updateSpec)
                      "SIP is revealed when a verdict on it was stably reached"
              $ cover 1 (fragment `votesSIP` updateSpec)
                      "SIP is voted when a verdict on it was stably reached"
              $ cover 1 (fragment `revealsImpl` updateSpec)
                      "Implementation is revealed when a verdict on its SIP was stably reached"
              $ cover 1 (fragment `votesImpl` updateSpec)
                      "Implementation is voted when a verdict on its SIP was stably reached"
              $ property ()
            checkEventCoverage (E (Implementation Submitted) fragment)
              = -- Chances that we can see this state in a trace are quite small
                -- since there is a high probability that the implementation
                -- commit is submitted before the SIP is approved. The system
                -- cannot reject this submission, so that's why this state has a
                -- low probability of being reached.
                --
                -- Note however that 0.1% coverage means that if we generate 1K
                -- traces, there will be around 10 traces in which this state is
                -- reached.
                cover 0.05 (fragment `submitsSIP` updateSpec)
                        "SIP is submitted when its implementation was submitted"
              $ cover 0.05 (fragment `revealsSIP` updateSpec)
                        "SIP is revealed when its implementation was submitted"
              $ cover 0.05 (fragment `votesSIP` updateSpec)
                        "SIP is voted when its implementation was submitted"
              $ cover 0.05 (fragment `submitsImpl` updateSpec)
                         "Implementation is submitted when it is already in the submitted state"
              $ cover 0.05 (fragment `votesImpl` updateSpec)
                        "Implementation is voted in the submitted state"
              $ property ()
            checkEventCoverage (E (Implementation StablySubmitted) fragment)
              = cover 1 (fragment `submitsSIP` updateSpec)
                        "SIP is submitted when its implementation was stably submitted"
              $ cover 1 (fragment `revealsSIP` updateSpec)
                        "SIP is revealed when its implementation was stably submitted"
              $ cover 1 (fragment `votesSIP` updateSpec)
                        "SIP is voted when its implementation was stably submitted"
              $ cover 1 (fragment `submitsImpl` updateSpec)
                         "Implementation is submitted when it is already in the stably submitted state"
              $ cover 1 (fragment `votesImpl` updateSpec)
                        "Implementation is voted in the stably submitted state"
              $ property ()
            checkEventCoverage (E (Implementation Revealed) fragment)
              = cover 1 (fragment `submitsSIP` updateSpec)
                        "SIP is submitted when its implementation was revealed"
              $ cover 1 (fragment `revealsSIP` updateSpec)
                        "SIP is revealed when its implementation was revealed"
              $ cover 1 (fragment `votesSIP` updateSpec)
                        "SIP is voted when its implementation was revealed"
              $ cover 1 (fragment `submitsImpl` updateSpec)
                         "Implementation is submitted when it is already revealed"
              $ cover 1 (fragment `votesImpl` updateSpec)
                        "Implementation is voted in the revealed state"
              $ property ()
            checkEventCoverage (E (Implementation StablyRevealed) fragment)
              = cover 1 (fragment `submitsSIP` updateSpec)
                        "SIP is submitted when its implementation was stably revealed"
              $ cover 1 (fragment `revealsSIP` updateSpec)
                        "SIP is revealed when its implementation was stably revealed"
              $ cover 1 (fragment `votesSIP` updateSpec)
                        "SIP is voted when its implementation was stably revealed"
              $ cover 1 (fragment `submitsImpl` updateSpec)
                         "Implementation is submitted when it is already in the stably revealed state"
              $ property ()
            checkEventCoverage (E (Implementation (Is _)) fragment)
              = cover 1 (fragment `submitsSIP` updateSpec)
                        "SIP is submitted when a verdict was reached on its implementation"
              $ cover 1 (fragment `revealsSIP` updateSpec)
                        "SIP is revealed a verdict was reached on its implementation"
              $ cover 1 (fragment `votesSIP` updateSpec)
                        "SIP is voted when a verdict was reached on its implementation"
              $ cover 1 (fragment `submitsImpl` updateSpec)
                         "Implementation is submitted when a verdict on it was reached"
              $ cover 1 (fragment `votesImpl` updateSpec)
                        "Implementation is voted when a verdict on it was reached"
              $ property ()
            checkEventCoverage (E (Implementation (IsStably _)) fragment)
              = cover 1 (fragment `submitsSIP` updateSpec)
                        "SIP is submitted when a verdict was stably reached on its implementation"
              $ cover 1 (fragment `revealsSIP` updateSpec)
                        "SIP is revealed a verdict was stably reached on its implementation"
              $ cover 1 (fragment `votesSIP` updateSpec)
                        "SIP is voted when a verdict was stably reached on its implementation"
              $ cover 1 (fragment `submitsImpl` updateSpec)
                         "Implementation is submitted when a verdict on it was stably reached"
              $ cover 1 (fragment `votesImpl` updateSpec)
                        "Implementation is voted when a verdict on it was stably reached"
              $ property ()
            checkEventCoverage (E Queued fragment)
              = cover 0.4 True "Queued"
                -- The generators generate about 1% of proposals being endorsed,
                -- so we expect the chance of seeing the invalid actions below
                -- being even smaller.
              $ cover 0.2 (fragment `submitsSIP` updateSpec)
                        "SIP is submitted when its implementation is queued"
              $ cover 0.2 (fragment `revealsSIP` updateSpec)
                        "SIP is revealed when its implementation is queued"
              $ cover 0.2 (fragment `votesSIP` updateSpec)
                        "SIP is voted when its implementation is queued"
              $ cover 0.2 (fragment `submitsImpl` updateSpec)
                         "Implementation is submitted when its implementation is queued"
              $ cover 0.2 (fragment `revealsImpl` updateSpec)
                         "Implementation is revealed when its implementation is queued"
              $ cover 0.2 (fragment `votesImpl` updateSpec)
                        "Implementation is voted when its implementation is queued"
              $ property ()
            checkEventCoverage (E BeingEndorsed fragment)
              = cover 0.4 True "Being endorsed"
              -- The generators generate about 1% of proposals being endorsed,
              -- so we expect the chance of seeing the invalid actions below
              -- being even smaller.
              $ cover 0.2 (fragment `submitsSIP` updateSpec)
                        "SIP is submitted when its implementation is being endorsed"
              $ cover 0.2 (fragment `revealsSIP` updateSpec)
                        "SIP is revealed when its implementation is being endorsed"
              $ cover 0.2 (fragment `votesSIP` updateSpec)
                        "SIP is voted when its implementation is being endorsed"
              $ cover 0.2 (fragment `submitsImpl` updateSpec)
                         "Implementation is submitted when its implementation is being endorsed"
              $ cover 0.2 (fragment `revealsImpl` updateSpec)
                         "Implementation is revealed when its implementation is being endorsed"
              $ cover 0.2 (fragment `votesImpl` updateSpec)
                        "Implementation is voted when its implementation is being endorsed"
              $ property ()
            checkEventCoverage (E ActivationCanceled fragment)
              = cover 0.01 True "Canceled"
              $ cover 0.005 (fragment `submitsSIP` updateSpec)
                        "SIP is submitted when its implementation is canceled"
              $ cover 0.005 (fragment `revealsSIP` updateSpec)
                        "SIP is revealed when its implementation is canceled"
              $ cover 0.005 (fragment `votesSIP` updateSpec)
                        "SIP is voted when its implementation is canceled"
              $ cover 0.005 (fragment `submitsImpl` updateSpec)
                         "Implementation is submitted when its implementation is canceled"
              $ cover 0.005 (fragment `revealsImpl` updateSpec)
                         "Implementation is revealed when its implementation is canceled"
              $ cover 0.005 (fragment `votesImpl` updateSpec)
                        "Implementation is voted when its implementation is canceled"
              $ property ()
            checkEventCoverage (E ActivationUnsupported fragment)
              = cover 0.01 True "Unsupported"
                -- The generators generate about 1% of proposals being endorsed,
                -- so we expect the chance of seeing the invalid actions below
                -- being even smaller.
              $ cover 0.005 (fragment `submitsSIP` updateSpec)
                        "SIP is submitted when its implementation is unsupported"
              $ cover 0.005 (fragment `revealsSIP` updateSpec)
                        "SIP is revealed when its implementation is unsupported"
              $ cover 0.005 (fragment `votesSIP` updateSpec)
                        "SIP is voted when its implementation is unsupported"
              $ cover 0.005 (fragment `submitsImpl` updateSpec)
                         "Implementation is submitted when its implementation is unsupported"
              $ cover 0.005 (fragment `revealsImpl` updateSpec)
                         "Implementation is revealed when its implementation is unsupported"
              $ cover 0.005 (fragment `votesImpl` updateSpec)
                        "Implementation is voted when its implementation is unsupported"
              $ property ()
            checkEventCoverage (E Scheduled fragment)
              = cover 0.01 True "Scheduled"
              $ cover 0.005 (fragment `submitsSIP` updateSpec)
                        "SIP is submitted when its implementation is scheduled"
              $ cover 0.005 (fragment `revealsSIP` updateSpec)
                        "SIP is revealed when its implementation is scheduled"
              $ cover 0.005 (fragment `votesSIP` updateSpec)
                        "SIP is voted when its implementation is scheduled"
              $ cover 0.005 (fragment `submitsImpl` updateSpec)
                         "Implementation is submitted when its implementation is scheduled"
              $ cover 0.005 (fragment `revealsImpl` updateSpec)
                         "Implementation is revealed when its implementation is scheduled"
              $ cover 0.005 (fragment `votesImpl` updateSpec)
                        "Implementation is voted when its implementation is scheduled"
              $ property ()
            checkEventCoverage (E ActivationExpired fragment)
              = cover 0.01 True "Activation expired"
              $ cover 0.005 (fragment `submitsSIP` updateSpec)
                        "SIP is submitted when its implementation is activation expired"
              $ cover 0.005 (fragment `revealsSIP` updateSpec)
                        "SIP is revealed when its implementation is activation expired"
              $ cover 0.005 (fragment `votesSIP` updateSpec)
                        "SIP is voted when its implementation is activation expired"
              $ cover 0.005 (fragment `submitsImpl` updateSpec)
                         "Implementation is submitted when its implementation is activation expired"
              $ cover 0.005 (fragment `revealsImpl` updateSpec)
                         "Implementation is revealed when its implementation is activation expired"
              $ cover 0.005 (fragment `votesImpl` updateSpec)
                        "Implementation is voted when its implementation is activation expired"
              $ property ()
            checkEventCoverage (E Activated fragment)
              = cover 0.01 True "Activated"
              $ cover 0.005 (fragment `submitsSIP` updateSpec)
                        "SIP is submitted when its implementation is activated"
              $ cover 0.005 (fragment `revealsSIP` updateSpec)
                        "SIP is revealed when its implementation is activated"
              $ cover 0.005 (fragment `votesSIP` updateSpec)
                        "SIP is voted when its implementation is activated"
              $ cover 0.005 (fragment `submitsImpl` updateSpec)
                         "Implementation is submitted when its implementation is activated"
              $ cover 0.005 (fragment `revealsImpl` updateSpec)
                         "Implementation is revealed when its implementation is activated"
              $ cover 0.005 (fragment `votesImpl` updateSpec)
                        "Implementation is voted when its implementation is activated"
              $ property ()
            checkEventCoverage (E _  _) = property ()

submitsSIP :: TraceFragment UpdateSUT -> UpdateSpec -> Bool
submitsSIP = fragmentContains getSubmittedSIP getSIPSubmission

revealsSIP :: TraceFragment UpdateSUT -> UpdateSpec -> Bool
revealsSIP = fragmentContains getRevealedSIP getSIPRevelation

votesSIP :: TraceFragment UpdateSUT -> UpdateSpec -> Bool
votesSIP = fragmentContains getVotedSIP getSIPId

submitsImpl :: TraceFragment UpdateSUT -> UpdateSpec -> Bool
submitsImpl = fragmentContains getSubmittedImpl getImplSubmission

revealsImpl :: TraceFragment UpdateSUT -> UpdateSpec -> Bool
revealsImpl = fragmentContains getRevealedImpl getImplRevelation

votesImpl :: TraceFragment UpdateSUT -> UpdateSpec -> Bool
votesImpl = fragmentContains getVotedImpl getImplId

fragmentContains
  :: (Eq a, IsTraceFragment f s)
  =>(SUTAct s -> Maybe a) -> (t -> a) -> f -> t -> Bool
fragmentContains fact fspec frag spec
  = any (\act -> fact act == Just (fspec spec)) $ invalidActions frag
