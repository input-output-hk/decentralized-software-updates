{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import           Test.Tasty.Ingredients.ConsoleReporter (UseColor (Auto))

import qualified Test.Cardano.Ledger.Update.Properties as Properties
import qualified Test.Cardano.Ledger.Update.Properties.Coverage as Properties.Coverage
import qualified Test.Cardano.Ledger.Update.UnitTests.Activation as UnitTests.Activation
import qualified Test.Cardano.Ledger.Update.UnitTests.Approval as UnitTests.Approval
import qualified Test.Cardano.Ledger.Update.UnitTests.Ideation as UnitTests.Ideation

main :: IO ()
main = defaultMain tests
 where
  tests :: TestTree
  tests = localOption Auto $ testGroup
    "Update"
    [ testGroup "💡 Ideation phase unit tests"  UnitTests.Ideation.runTests
    , testGroup "👍 Approval phase unit tests"   UnitTests.Approval.runTests
    , testGroup "⚡ Activation phase unit tests" UnitTests.Activation.runTests
    , testGroup "🎰 Property tests"             Properties.runTests
    , testGroup "☂ Coverage tests"              Properties.Coverage.runTests
    ]
