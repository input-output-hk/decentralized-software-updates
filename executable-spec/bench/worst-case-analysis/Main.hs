module Main where

import           Cardano.Ledger.Benchmarks.Update.WorstCaseAnalysis
import 			 Cardano.Ledger.MBenchmarks.Update.Tally.TallyMicroBenchmark 

import qualified  Criterion.Main as Cr

main :: IO ()
main = do
  --putStrLn worstCaseAnalysisTable
  --putStrLn worstCaseAnalysisPlot
  putStrLn $ show $ runTally $ BenchmarkParams 1000 10
  Cr.defaultMain [	Cr.bgroup "tally" [ Cr.bench "1"    $ Cr.whnf runTally (BenchmarkParams 1 10)
				                      , Cr.bench "10"   $ Cr.whnf runTally (BenchmarkParams 10 10)
				                      , Cr.bench "100"  $ Cr.whnf runTally (BenchmarkParams 100 10)
				                      , Cr.bench "1000" $ Cr.whnf runTally (BenchmarkParams 1000 10)
				                      ]
  				 ]

