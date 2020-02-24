{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Exception (assert)

import           Ledger.Core (Slot (Slot))

import           Cardano.Ledger.Benchmarks.Update.Tally
import           Cardano.Ledger.Spec.State.ProposalState (Decision (Accepted))

import qualified Criterion.Main as Cr

main :: IO ()
main = do
  let !tallyData5 =
        createTallyData constants (NumberOfParticipants 10000)  (NumberOfConcurrentUPs 10)
      !tallyData6 =
        createTallyData constants (NumberOfParticipants 100000) (NumberOfConcurrentUPs 10)
  print $ runTally constants tallyData5
  Cr.defaultMain
    [ Cr.bgroup "tally" [ Cr.bench "1e5" $ Cr.whnf allApproved tallyData5
                        , Cr.bench "1e6" $ Cr.whnf allApproved tallyData6
                        ]
    ]
  where
    !constants =
      BenchmarkConstants
      { k              = 1
      , r_a            = 0.49
      , revelationSlot = Slot 0
      }
    allApproved tallyData
      = all (== Accepted) $ runTally constants tallyData
