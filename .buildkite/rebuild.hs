{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import           Build (LibraryName (LibraryName), Optimizations (Fast),
                     TestArguments (TestArguments), TestRuns (TestRuns),
                     doBuild)
import           BuildArgs (BuildArgs (BuildArgs, command, options),
                     Command (Build, CleanupCache, PurgeCache),
                     RebuildOpts (RebuildOpts, optBuildDirectory, optCacheDirectory, optDryRun),
                     parseArgs)
import           CommonBuild (CoverallsConfig (CoverallsConfig),
                     CoverallsTokenEnvVar (CoverallsTokenEnvVar),
                     ExtraShcArgs (ExtraShcArgs), IO,
                     TixDirectory (TixDirectory))

import           Data.Maybe (fromMaybe)
import           System.Exit (exitWith)

import qualified Data.Text as T


main :: IO ()
main =
  doBuild
    (LibraryName "decentralized-software-updates")
    Standard
    (TestRuns [TestArguments []])
    (CoverallsConfig
       (CoverallsTokenEnvVar "")
       (ExtraShcArgs [])
       (TixDirectory ".")
    )
