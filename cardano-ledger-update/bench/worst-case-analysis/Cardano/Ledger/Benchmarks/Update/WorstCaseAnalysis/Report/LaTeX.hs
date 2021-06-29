{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Ledger.Benchmarks.Update.WorstCaseAnalysis.Report.LaTeX where

import           Data.List (intercalate)

type HeaderLabel = String

data Value
  = forall v . (Show v) => Value v
  | StringValue String

showValue :: Value -> String
showValue (Value v) = show v
showValue (StringValue v) = v

latexTable
  :: [HeaderLabel]
  -> [[Value]]
  -> String
latexTable headerLabels resultss
  =  header
  ++ concatMap printResults resultss
  ++ footer
  where
    header =  tab ++ begin "tabular" ++ mkAlignment (length headerLabels) ++ eol
           ++ tab ++ tab ++ hline ++ eol
           ++ tab ++ tab ++ mkHeaderLabels headerLabels ++ newline ++ eol
           ++ tab ++ tab ++ hline ++ eol
    printResults results
      = tab ++ tab ++ intercalate " & " (fmap showValue results) ++ newline ++ eol
    footer =  tab ++ tab ++ hline ++ eol
           ++ tab ++ end "tabular" ++ eol

tab :: String
tab = "  " -- for now tab width is not configurable.

begin :: String -> String
begin env = "\\begin{" ++ env ++ "}"

end :: String -> String
end env = "\\end{" ++ env ++ "}"

eol :: String
eol = "\n"

hline :: String
hline = "\\hline"

newline :: String
newline = "\\\\"

mkHeaderLabels :: [HeaderLabel] -> String
mkHeaderLabels headerLabels =
  intercalate " & " headerLabels

mkAlignment :: Int -> String
mkAlignment n
  =  "{| "
  ++ intercalate " | " (replicate n "r")
  ++ " |}"

mathenv :: String -> String
mathenv str = "$" ++ str ++ "$"

mathit :: String -> String
mathit str = "\\mathit{" ++ str ++ "}"


data PlotScale = Linear | Logarithmic deriving (Show, Eq)

scaleSpec :: PlotScale -> String
scaleSpec Linear = "linear"
scaleSpec Logarithmic = "log"

data PlotParams =
  PlotParams
  { title        :: String
  , xLabel       :: String
  , xMin         :: Double
  , xMax         :: Double
  , xScale       :: PlotScale
  , xTicks       :: [Double]
  , yLabel       :: String
  , yMin         :: Double
  , yMax         :: Double
  , yScale       :: PlotScale
  , yTicks       :: [Double]
  , dataFileName :: String
  }
  deriving (Show)

(.=) :: String -> String -> String
key .= val = key ++ "=" ++ embrace val

embrace :: String -> String
embrace str = "{" ++ str ++ "}"

pgfPlot
  :: PlotParams
  -> [(Double, Double)]
  -- ^ Coordinates (x, y)
  -> String
pgfPlot PlotParams
        { title
        , xLabel
        , xMin
        , xMax
        , xScale
        , xTicks
        , yLabel
        , yMin
        , yMax
        , yScale
        , yTicks
        , dataFileName
        }
        coordinates
  =  tab ++ begin "tikzpicture" ++ eol
  ++ tab ++ tab ++ begin "axis" ++ "[\n"
  ++ tab ++ tab ++ tab ++ "title" .= title ++ ",\n"
  ++ tab ++ tab ++ tab ++ "xlabel" .= xLabel ++ ",\n"
  ++ tab ++ tab ++ tab ++ "xmin=" ++ show xMin ++ ",\n"
  ++ tab ++ tab ++ tab ++ "xmax=" ++ show xMax ++ ",\n"
  ++ tab ++ tab ++ tab ++ "xmode=" ++ scaleSpec xScale ++ ",\n"
  ++ tab ++ tab ++ tab ++ "xtick" .= intercalate ", " (fmap show xTicks) ++ "," ++ eol
  ++ tab ++ tab ++ tab ++ "ylabel" .= yLabel ++ ",\n"
  ++ tab ++ tab ++ tab ++ "ymin=" ++ show yMin ++ ",\n"
  ++ tab ++ tab ++ tab ++ "ymax=" ++ show yMax ++ ",\n"
  ++ tab ++ tab ++ tab ++ "ymode=" ++ scaleSpec yScale ++ ",\n"
  ++ tab ++ tab ++ tab ++ "ytick" .= intercalate ", " (fmap show yTicks) ++ ",\n"
  ++ tab ++ tab ++ tab ++ "legend pos=north west,\n"
  ++ tab ++ tab ++ tab ++ "ymajorgrids=true,\n"
  ++ tab ++ tab ++ tab ++ "grid style=dashed,\n"
  ++ tab ++ tab ++ tab ++ "]\n"
  ++ tab ++ tab ++ tab ++ "\\addplot[color=black] table " ++ embrace dataFileName ++ ";\n"
  ++ tab ++ tab ++ end "axis" ++ eol
  ++ tab ++ end "tikzpicture" ++ eol
  -- We dump next the contents of the data file
  ++ "\n--------------------------------------------------------------------------------\n"
  ++ "Data file contents: \n\n"
  ++ intercalate "\n" (fmap showPair coordinates) ++ eol
  ++ "\n--------------------------------------------------------------------------------\n"
  where
    showPair (x, y) = show x ++ " " ++ show y
