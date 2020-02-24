module Main where

import           Cardano.Ledger.Benchmarks.Update.Tally

import qualified Criterion.Main as Cr

main :: IO ()
main = do
  -- putStrLn $ show $ runTally $ createBenchmarkData $ BenchmarkParams 100000 10
  let b1 = createBenchmarkData (BenchmarkParams 1 10)
      b10 = createBenchmarkData (BenchmarkParams 10 10)
      b100 = createBenchmarkData (BenchmarkParams 100 10)
      b1000 = createBenchmarkData (BenchmarkParams 1000 10)
      b10000 = createBenchmarkData (BenchmarkParams 10000 10)
      b100000 = createBenchmarkData (BenchmarkParams 100000 10)
      b1000000 = createBenchmarkData (BenchmarkParams 1000000 10)
--      b10000000 = createBenchmarkData (BenchmarkParams 10000000 10)
  Cr.defaultMain [	Cr.bgroup "tally" [ Cr.bench "1"    $ Cr.nf runTally $ b1
				                      , Cr.bench "10"   $ Cr.nf runTally $ b10
				                      -- , Cr.bench "100"  $ Cr.nf runTally $ b100
				                      -- , Cr.bench "1000" $ Cr.nf runTally $ b1000
 				                      -- , Cr.bench "10000" $ Cr.nf runTally $ b10000
 				                      -- , Cr.bench "100000" $ Cr.nf runTally $ b100000
				                      -- , Cr.bench "1000000" $ Cr.nf runTally $ b1000000
--				                      , Cr.bench "10000000" $ Cr.nf runTally $ b10000000
				                      ]
  				 ]
