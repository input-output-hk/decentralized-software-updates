#!/usr/bin/env stack
-- stack --resolver lts-15.3 script --package turtle --package text --package optparse-generic
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

-- | Odd chain demo script.
--
-- This script requires that tmux is installed, and must be run inside a tmux
-- session.
import           Turtle
import           Control.Exception (assert)
import           Control.Monad (replicateM, unless)
import           Data.Maybe (isJust)
import qualified Data.Text as T
import           Options.Generic (ParseRecord,
                     unwrapRecord, type (<?>), (:::), Wrapped, Unwrapped)
import           GHC.Generics (Generic)

data Args w =
  Args
  { numNodes :: w ::: Int <?> "Number of core nodes and clients to run."
  } deriving (Generic)

instance ParseRecord (Args Wrapped)
deriving instance Show (Args Unwrapped)

main :: IO ExitCode
main = do
  -- Check that @tmux@ is installed
  mPath <- which "tmux"
  unless (isJust mPath) $ die "Can't find tmux"
  --
  -- TODO get the current time and use this as the 'startTimeString' parameter
  --
  currentTime <- date
  -- Strip the picoseconds part, to make @oddnode@ understand the data.
  let networkStartTimeString = show (currentTime :: UTCTime)

  putStrLn $ "Starting network at " ++ networkStartTimeString

  Args { numNodes } <- unwrapRecord "Odd chain demo"

  -- Splitting a screen in more than 6 panes is not supported.
  unless (1 <= numNodes && numNodes < 4) $
    die "Number of nodes needs to be between 1 and 3."

  -- Create one horizontal pane per process.
  assert (1 <= numNodes && numNodes < 4) $
    replicateM (numNodes - 1) (sh splitHorizontal)

  -- Make the layout even.
  sh "tmux select-layout even-horizontal"

  -- Split each horizontal pane vertically. Splitting a pane will cause the
  -- number of the existing panes to change. E.g. given the layout:
  --
  -- > | 0 | 1 |
  -- > |   |   |
  --
  -- Splitting 0 will cause the panes to be renumbered as follows:
  --
  -- > | 0 | 2 |
  -- > | 1 |   |
  --
  -- In this way, if we start splitting at the first pane, the resulting
  -- horizontal panes numbers will consist of consecutive even numbers.
  traverse (sh . splitVertical) $ fmap (2*) [0..numNodes - 1]
  -- The code above should be equivalent to running:
  --
  -- > sh (splitVertical 0)
  -- > sh (splitVertical 2)
  -- > ...
  --

  -- Start sending stack commands to each terminal.
  --
  -- First run the nodes ...
  traverse (sh . runNode networkStartTimeString numNodes) $ fmap (2*) [0 .. numNodes - 1]
  -- Then the clients (wallets).
  traverse (sh . runWallet) $ fmap ((1+) . (2*)) [0 .. numNodes - 1]
  exit ExitSuccess
  where
    sh cmd = shell cmd empty
    splitHorizontal = "tmux split-window -d -t 0 -h"
    splitVertical paneNumber = "tmux split-window -d -t "
                             <> T.pack (show paneNumber)
                             <> " -v"

    runNode startTimeString numberOfNodes paneNumber
      =  "tmux send-keys -t "
      <> T.pack (show paneNumber)
      <> " \""
      <> "stack exec oddnode --"
      <> " --numberOfCoreNodes " <> T.pack (show numberOfNodes)
      <> " --nodeId " <> T.pack (show $ paneNumber `div` 2)
      <> " --enableTracers Consensus"
      <> " --startTimeString '" <> T.pack startTimeString <> "'"
      <> " \""
      <> " Enter"

    runWallet paneNumber =  "tmux send-keys -t "
                       <> T.pack (show paneNumber)
                       <> " \""
                       -- Wait a bit till the client start (the odd wallet is not fault tolerant).
                       <> "sleep 5; "
                       <> "stack exec oddwallet --"
                       <> " --nodeId " <> T.pack (show $ paneNumber `div` 2)
                       <> " --enabledTracers Rolls"
                       <> " \""
                       <> " Enter"
