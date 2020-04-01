module Main where

import           Test.Tasty (defaultMain, testGroup)

import qualified Datil.Examples.Echo as Echo
import qualified Datil.Examples.FarmerCrossesRiver as Farmer
import qualified Datil.Examples.PingPong as PingPong

main :: IO ()
main = defaultMain tests
  where
    tests =
      testGroup "Examples"
        [ testGroup "Unit tests"
            [ PingPong.tests
            , Echo.tests
            , Farmer.tests
            ]
        , testGroup "Properties"
            [ PingPong.propertyTests
            , Echo.propertyTests
            , Farmer.propertyTests
            ]
        ]
