module Main where

import           Cardano.Ledger.Benchmarks.Update.WorstCaseAnalysis

main :: IO ()
main = do
  putStrLn worstCaseAnalysisTable
  putStrLn worstCaseAnalysisPlot
