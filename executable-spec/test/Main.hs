{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.Ingredients.ConsoleReporter (UseColor (Auto))

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
            Ideation.onlyValidSignalsAreGenerated
        ]
    , testGroup "Chain properties"
      [ testProperty
          "Only valid traces are generated"
          Chain.onlyValidSignalsAreGenerated
      ,  testProperty
          "Traces are classified"
          Chain.tracesAreClassified
      ]
    ]
