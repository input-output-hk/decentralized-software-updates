{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import           Test.Tasty.Ingredients.ConsoleReporter (UseColor (Auto))
import           Test.Tasty.QuickCheck (testProperty)

import qualified Cardano.Ledger.Spec.STS.Chain.Chain.Properties as Chain
import qualified Cardano.Ledger.Spec.STS.SM.Update.Ideation.Full.Properties as SM.Full
import qualified Cardano.Ledger.Spec.STS.SM.Update.Ideation.Properties as SM.Ideation
import qualified Cardano.Ledger.Spec.STS.SM.Update.Ideation.Properties.Conformance as Ideation.Conformance
import qualified Cardano.Ledger.Spec.STS.SM.Update.Ideation.Vote.Properties as SM.Vote
import           Cardano.Ledger.Spec.STS.Update.Approval.UnitTests as Approval.UnitTests
import qualified Cardano.Ledger.Spec.STS.Update.Ideation.Properties as Ideation

main :: IO ()
main = defaultMain tests
 where
  tests :: TestTree
  tests = localOption Auto $ testGroup
    "Update"
    [ testGroup "Ideation phase unit tests" []
    , testGroup "Ideation phase properties"
        [ testProperty
            "Only valid traces are generated"
            Ideation.qc_onlyValidSignalsAreGenerated
        ]
    , testGroup "Chain properties"
      [ testProperty
          "Only valid traces are generated"
          Chain.qc_onlyValidSignalsAreGenerated
      , testProperty
          "Trace reveals are classified"
          Chain.qc_revealsAreClassified
      , testProperty
          "Appropriate Traces are covered"
          (Chain.relevantCasesAreCovered)
      ]
    , testGroup "State machine properties"
      [ testGroup "Ideation: submit reveal"
        [ testGroup "Unit tests"
          [ testProperty
              "Example 0"
              SM.Ideation.example0
          , testProperty
              "Example 1"
              SM.Ideation.example1
          ]
        , testProperty
            "Reveals are generated"
            SM.Ideation.prop_noReveals
        , testProperty
            "Reveals correspond to stable submissions"
            SM.Ideation.prop_revealsStableSubmissions
        ]
      , testGroup "Ideation: voting"
        [ testGroup "Unit tests"
          [ testProperty
              "Example 0"
              SM.Vote.example0
          , testProperty
              "Example 1"
              SM.Vote.example1
          ]
        , testGroup "Properties"
          [ testProperty
              "Tally events are generated"
              SM.Vote.prop_noTally
          , testProperty
              "Approval events are generated"
              SM.Vote.prop_eventuallyApproval
          , testProperty
              "Rejection events are generated"
              SM.Vote.prop_eventuallyRejection
          ]
        ]
      , testGroup "Ideation: full"
        [ testProperty
            "Single SIP eventual approval"
            SM.Full.prop_eventuallyApproval
        , testProperty
            "Single SIP eventual rejection"
            SM.Full.prop_eventuallyRejection
        , testProperty
            "Multiple SIP eventual approval"
            SM.Full.prop_eventuallyApprovalN
        , testProperty
            "Sufficient invalid actions are generated"
            SM.Full.prop_sufficientInvalidActions
        ]
      , testGroup "Coverage"
        [ testProperty
            "A sufficient number of approvals is generated"
            SM.Full.prop_sufficientApprovals
        , testProperty
            "A sufficient number of rejections is generated"
            SM.Full.prop_sufficientRejections
        , testProperty
            "A sufficient number of no-verdicts is generated"
            SM.Full.prop_sufficientNoVerdicts
        ]
      , testGroup "Conformance"
        [ testProperty
            "Ideation SOS rules conform to SM model"
            Ideation.Conformance.prop_updateConforms
        ]
      ]
    , testGroup "Approval phase unit tests" Approval.UnitTests.runTests
    ]
