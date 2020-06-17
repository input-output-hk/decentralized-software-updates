{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | To test this you can run:
--
-- > nix-shell .buildkite --run "ghci .buildkite/rebuild.hs"
--

import Build
    ( LibraryName (LibraryName)
    , Optimizations (Fast)
    , StackExtraTestArgs (StackExtraTestArgs)
    , TestRun (TestRun)
    , doBuild
    , Timeout (Timeout)
    , uploadCoverageIfBors
    )
import BuildArgs
    ( BuildArgs (BuildArgs, command, options)
    , Command (Build, CleanupCache, PurgeCache)
    , RebuildOpts (RebuildOpts, optBuildDirectory, optCacheDirectory, optDryRun)
    , parseArgs
    )
import CommonBuild
    ( CoverallsConfig (CoverallsConfig)
    , CoverallsTokenEnvVar (CoverallsTokenEnvVar)
    , ExtraShcArgs (ExtraShcArgs)
    , ExtraTixFilesDirectory (ExtraTixFilesDirectory)
    , IO
    , const
    , ($)
    )

import Data.Maybe
    ( fromMaybe )
import System.Exit
    ( exitWith )

import qualified Data.Text as T


main :: IO ()
main =
  doBuild
    (LibraryName "decentralized-software-updates")
    Fast
    uploadCoverageIfBors
    [TestRun $ StackExtraTestArgs $ const []]
    (CoverallsConfig
       (CoverallsTokenEnvVar "DSU_COVERALLS_TOKEN")
       (ExtraShcArgs [])
       (ExtraTixFilesDirectory "lib")
    )
    (Timeout 100)
