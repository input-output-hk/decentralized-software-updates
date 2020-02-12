module Main where

import           Cardano.Ledger.Benchmarks.Update.WorstCaseAnalysis
import 			 Cardano.Ledger.MBenchmarks.Update.Tally.TallyMicroBenchmark 

main :: IO ()
main = do
  --putStrLn worstCaseAnalysisTable
  --putStrLn worstCaseAnalysisPlot
  putStrLn $ show $ runTally $ BenchmarkParams 1000 10

