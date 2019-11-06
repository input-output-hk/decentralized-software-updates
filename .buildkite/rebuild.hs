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
                     QALevel (FullTest), TestArguments (TestArguments),
                     TestRun (TestRun), doBuild)
import           CommonBuild (CoverallsConfig (CoverallsConfig),
                     CoverallsTokenEnvVar (CoverallsTokenEnvVar),
                     ExtraShcArgs (ExtraShcArgs), IO,
                     TixDirectory (TixDirectory), const)


main :: IO ()
main =
  doBuild
    (LibraryName "decentralized-software-updates")
    Standard
    (const FullTest)
    [TestRun (TestArguments []) []]
    (CoverallsConfig
       (CoverallsTokenEnvVar "DSU_COVERALLS_TOKEN")
       (ExtraShcArgs [])
       (TixDirectory ".")
    )
