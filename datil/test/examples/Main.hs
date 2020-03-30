module Main where

import           Test.Tasty (defaultMain, testGroup)

import qualified Datil.Examples.Echo as Echo
import qualified Datil.Examples.PingPong as PingPong

main :: IO ()
main = defaultMain tests
  where
    tests =
      testGroup "Examples"
      [ PingPong.tests
      , Echo.tests
      ]
