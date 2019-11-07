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

import           Build (LibraryName (LibraryName), Optimizations (Standard),
                     ShouldUploadCoverage (ShouldUploadCoverage),
                     StackExtraTestArgs (StackExtraTestArgs),
                     TestRun (TestRun), doBuild)
import           CommonBuild (Bool (True), CoverallsConfig (CoverallsConfig),
                     CoverallsTokenEnvVar (CoverallsTokenEnvVar),
                     ExtraShcArgs (ExtraShcArgs),
                     ExtraTixFilesDirectory (ExtraTixFilesDirectory), IO,
                     const, ($))


main :: IO ()
main =
  doBuild
    (LibraryName "decentralized-software-updates")
    Standard
    (ShouldUploadCoverage $ const True)
    [TestRun $ StackExtraTestArgs $ const []]
    (CoverallsConfig
       (CoverallsTokenEnvVar "DSU_COVERALLS_TOKEN")
       (ExtraShcArgs [])
       (ExtraTixFilesDirectory ".")
    )
