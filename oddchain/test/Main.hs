module Main (main) where

import           Test.Tasty

import qualified Test.OddChain as OddChain
import qualified Test.ThreadNet.OddChain as ThreadNet


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "oddchain"
  [ OddChain.tests
  , ThreadNet.tests
  ]
