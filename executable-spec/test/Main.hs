{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.Ingredients.ConsoleReporter (UseColor (Auto))

import qualified Test.Tasty.QuickCheck as Tasty.QuickCheck

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
            "Only valid traces are generated (Hedgehog)"
            Ideation.onlyValidSignalsAreGenerated
        , Tasty.QuickCheck.testProperty
            "Only valid traces are generated (QuickCheck)"
            Ideation.qc_onlyValidSignalsAreGenerated
        , Tasty.QuickCheck.testProperty
            "Trace lengths are classified (QuickCheck)"
            Ideation.qc_traceLengthsAreClassified
        ]
    , testGroup "Chain properties"
      [ testProperty
          "Only valid traces are generated"
          Chain.onlyValidSignalsAreGenerated
      ,  testProperty
          "Traces are classified"
          Chain.tracesAreClassified
      ,  testProperty
          "Traces update payload counts are classified"
          Chain.updatePayloadCountIsClassified
      , Tasty.QuickCheck.testProperty
          "Only valid traces are generated (QuickCheck)"
          Chain.qc_onlyValidSignalsAreGenerated
        , Tasty.QuickCheck.testProperty
            "Traces update payload counts are classified (QuickCheck)"
            Chain.qc_updatePayloadCountIsClassified
        , Tasty.QuickCheck.testProperty
            "Traces lengths are classified (QuickCheck)"
            Chain.qc_traceLengthsAreClassified
        , Tasty.QuickCheck.testProperty
            "Traces reveals are classified (QuickCheck)"
            Chain.qc_revealsAreClassified
      ]
    ]
