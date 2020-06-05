{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import           Test.Tasty.Ingredients.ConsoleReporter (UseColor (Auto))

import qualified Cardano.Ledger.Update.UnitTests.Activation as UnitTests.Activation
import qualified Cardano.Ledger.Update.UnitTests.Approval as UnitTests.Approval
import qualified Cardano.Ledger.Update.UnitTests.Ideation as UnitTests.Ideation

main :: IO ()
main = defaultMain tests
 where
  tests :: TestTree
  tests = localOption Auto $ testGroup
    "Update"
    [ testGroup "💡 Ideation phase unit tests" UnitTests.Ideation.runTests
    , testGroup "👍 Approval phase unit tests" UnitTests.Approval.runTests
    , testGroup "⚡ Activation phase unit tests" UnitTests.Activation.runTests
    ]
