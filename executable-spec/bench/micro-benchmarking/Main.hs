{-# LANGUAGE BangPatterns #-}

module Main where

import           Ledger.Core (Slot (Slot))

import           Cardano.Ledger.Benchmarks.Update.Tally
import           Cardano.Ledger.Spec.State.ProposalState (Decision (Accepted))

import qualified Criterion.Main as Cr

main :: IO ()
main = do
  let
      !tallyData2 =
        createTallyData constants (NumberOfParticipants 100)      (NumberOfConcurrentUPs 1)
      !tallyData3 =
        createTallyData constants (NumberOfParticipants 1000)     (NumberOfConcurrentUPs 1)
      !tallyData4 =
        createTallyData constants (NumberOfParticipants 10000)    (NumberOfConcurrentUPs 1)
      !tallyData5 =
        createTallyData constants (NumberOfParticipants 100000)   (NumberOfConcurrentUPs 1)
      !tallyData6 =
        createTallyData constants (NumberOfParticipants 1000000)  (NumberOfConcurrentUPs 1)
      !tallyData7 =
        createTallyData constants (NumberOfParticipants 10000000) (NumberOfConcurrentUPs 1)
  print $ runTally constants tallyData2
  Cr.defaultMain
    [ Cr.bgroup "tally" [ Cr.bench "1e2" $ Cr.whnf allApproved tallyData2
                        , Cr.bench "1e3" $ Cr.whnf allApproved tallyData3
                        , Cr.bench "1e4" $ Cr.whnf allApproved tallyData4
                        , Cr.bench "1e5" $ Cr.whnf allApproved tallyData5
                        , Cr.bench "1e6" $ Cr.whnf allApproved tallyData6
                        , Cr.bench "1e7" $ Cr.whnf allApproved tallyData7
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
      = if all (== Accepted) $ runTally constants tallyData
        then True
        else error "All proposals should be accepted!"
