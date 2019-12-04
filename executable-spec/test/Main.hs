{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import           Test.Tasty.Ingredients.ConsoleReporter (UseColor (Auto))
import           Test.Tasty.QuickCheck (testProperty)

import qualified Cardano.Ledger.Spec.STS.Chain.Chain.Properties as Chain
import qualified Cardano.Ledger.Spec.STS.Update.Ideation.Properties as Ideation

main :: IO ()
main = defaultMain tests
 where
  tests :: TestTree
  tests = localOption Auto $ testGroup
    "Update"
    [ testGroup "Ideation phase examples" []
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
    ]
