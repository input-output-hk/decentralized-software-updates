{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}

-- We declare orphan instances for 'OddChainArgs' so that we can define command
-- line parsers for values of this type.
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import           Control.Monad (when)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Options.Generic (ParseRecord, ParseField, ParseFields,
                     unwrapRecord, type (<?>), (:::), Wrapped, Unwrapped)
import           System.Directory (doesFileExist, removeFile)
import           System.Exit (die)
import           Data.Time.Format (parseTimeOrError, defaultTimeLocale)

import           OddNode (nodeLocalSocketPath, oddRun, NodeTracer,
                     RunConfig, mkRunConfig)


data OddChainArgs w =
  OddChainArgs
  { numberOfCoreNodes :: w ::: Word64       <?> "Number of core nodes."
  , nodeId            :: w ::: Word64       <?> "Node identifier"
  , enableTracers     :: w ::: [NodeTracer] <?> "Tracers to enable"
  , startTimeString   :: w ::: String       <?> "Network starting time"
  } deriving (Generic)

instance ParseRecord (OddChainArgs Wrapped)
deriving instance Show (OddChainArgs Unwrapped)

instance ParseRecord NodeTracer
instance ParseField NodeTracer
instance ParseFields NodeTracer

parseCommandLineOptions
  :: IO RunConfig
parseCommandLineOptions = do
  OddChainArgs { numberOfCoreNodes
               , nodeId
               , enableTracers
               , startTimeString   } <- unwrapRecord "Odd node"
  let
    timeFormat = "%Y-%-m-%-d %H:%M:%S%Q %Z"
    startTime  = parseTimeOrError True defaultTimeLocale timeFormat startTimeString
  putStrLn $ "Network started at " ++ show startTime
  if (numberOfCoreNodes  <= nodeId)
    then die "nodeId should be between 0 and numberOfCoreNodes - 1"
    else pure $! mkRunConfig numberOfCoreNodes nodeId enableTracers startTime

main :: IO ()
main = do
  config <- parseCommandLineOptions
  let socketPath = nodeLocalSocketPath config
  fileExists <- doesFileExist socketPath
  when fileExists $ removeFile socketPath
  oddRun config
