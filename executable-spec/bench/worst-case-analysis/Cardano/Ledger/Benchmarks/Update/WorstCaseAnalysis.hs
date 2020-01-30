{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Ledger.Benchmarks.Update.WorstCaseAnalysis where

import           Prelude hiding ((/), (^))
import qualified Prelude as Prelude

import           Text.Printf (printf)

import           Cardano.Ledger.Benchmarks.Update.WorstCaseAnalysis.Arithmetic
                     ((/))
import           Cardano.Ledger.Benchmarks.Update.WorstCaseAnalysis.PayloadSizes
import           Cardano.Ledger.Benchmarks.Update.WorstCaseAnalysis.Report.LaTeX
import           Cardano.Ledger.Benchmarks.Update.WorstCaseAnalysis.Units
import           Cardano.Ledger.Benchmarks.Update.WorstCaseAnalysis.Units.TBPS

-- | Duration of a single voting period, in seconds.
--
votePeriodDuration :: AnalysisParams -> Seconds
votePeriodDuration AnalysisParams { votingDays } = secondsInAWeek
  where
    secondsInAWeek = fromIntegral votingDays * 24 * 60 * 60

-- | Size of the votes payload for a given SIP.
--
votesPayloadSize :: AnalysisParams -> NumberOfBytes
votesPayloadSize AnalysisParams {participants, voteChanges}
  = voteSize * fromIntegral (participants * voteChanges)

data AnalysisParams =
  AnalysisParams
  { participants   :: Word
  -- ^ Number of participants.
  , voteChanges    :: Word
  -- ^ Number of times participants change their votes.
  , concurrentSIPs :: Word
  -- ^ SIP's that are active at the same time. We assume their voting period
  -- overlaps exactly, which is the worst case.
  , votingDays     :: Word
  -- ^ Days a vote takes.
  , systemTBPS     :: TBPS
  -- ^ TBPS of the system.
  }
  deriving (Show, Eq)

concurrentVotesPayloadSize :: AnalysisParams -> NumberOfBytes
concurrentVotesPayloadSize params@AnalysisParams { concurrentSIPs }
  = votesPayloadSize params `times` concurrentSIPs

usagePct :: AnalysisParams -> Double
usagePct params@AnalysisParams { systemTBPS }
  = (totalVotesPayloadSize / capacity systemTBPS (votePeriodDuration params))
  * 100
  where
    totalVotesPayloadSize = concurrentVotesPayloadSize params


--------------------------------------------------------------------------------
-- Tables and plots
--------------------------------------------------------------------------------

worstCaseAnalysisTable :: String
worstCaseAnalysisTable
  = latexTable (mathenv
                <$> [ "n_p"
                    , "n_r"
                    , "n_c"
                    , mathit "duration_v"
                    , mathit "usage_{pct}"
                    ]
               )
               (fmap runAnalysis paramss)
  where
    paramss = [ AnalysisParams 1000    2 1 7 _25k
              , AnalysisParams 1000    2 5 7 _25k
              , AnalysisParams 1000    2 10 7 _25k
              --
              , AnalysisParams 10000   2 1 7 _25k
              , AnalysisParams 10000   2 5 7 _25k
              , AnalysisParams 10000   2 10 7 _25k
              --
              , AnalysisParams 100000  2 1 7 _25k
              , AnalysisParams 100000  2 5 7 _25k
              , AnalysisParams 100000  2 10 7 _25k
              --
              , AnalysisParams 1000000  2 1 7 _25k
              , AnalysisParams 1000000  2 5 7 _25k
              , AnalysisParams 1000000  2 10 7 _25k
              , AnalysisParams 1000000  2 10 14 _25k
              , AnalysisParams 1000000  2 10 30 _25k
              --
              , AnalysisParams 10000000  2 1 7 _25k
              , AnalysisParams 10000000  2 5 7 _25k
              , AnalysisParams 10000000  2 10 7 _25k
              ]

    runAnalysis :: AnalysisParams -> [Value]
    runAnalysis params@AnalysisParams { participants
                                      , voteChanges
                                      , concurrentSIPs
                                      , votingDays
                                      }
      = [ Value participants
        , Value voteChanges
        , Value concurrentSIPs
        , Value votingDays
        , StringValue (printf "%.3f" (usagePct params) :: String)
        ]

_25k :: TBPS
_25k = tbps 25000 1

(^) :: Integer -> Integer -> Integer
a ^ b = a Prelude.^ b

worstCaseAnalysisPlot :: String
worstCaseAnalysisPlot
  = pgfPlot PlotParams
            { title = "Participants vs usage percentage"
            , xLabel = "Participants"
            , xMin   = 100
            , xMax   = fromIntegral $ 10^7
            , xScale = Logarithmic
            , xTicks = fmap (fromIntegral . (10 ^))  [1..7]
            , yLabel = "Usage percentage"
            , yMin = 0
            , yMax = 250
            , yScale = Linear
            , yTicks = [5, 20, 50, 100, 150, 200, 250]
            , dataFileName = "participants-vs-usage.dat"
            }
            dataPoints
  where
    paramss = fmap mkAnalysisParams samples
      where
        mkAnalysisParams participants = AnalysisParams participants 2 10 7 _25k
    samples =  fmap (100*) [1 .. 10]
            ++ fmap (1000 *) [1 .. 10000]
    results = fmap usagePct paramss
    dataPoints = zip (fmap fromIntegral samples) results
