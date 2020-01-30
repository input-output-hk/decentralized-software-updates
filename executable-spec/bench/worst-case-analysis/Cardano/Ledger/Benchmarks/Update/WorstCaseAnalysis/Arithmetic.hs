
module Cardano.Ledger.Benchmarks.Update.WorstCaseAnalysis.Arithmetic where

(/) :: Integral a => a -> a -> Double
x / y = asDouble x Prelude./ asDouble y

asDouble :: Integral a => a -> Double
asDouble = fromIntegral
