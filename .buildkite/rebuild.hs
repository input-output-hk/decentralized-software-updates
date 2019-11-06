{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import           Build (LibraryName (LibraryName), Optimizations (Standard),
                     TestArguments (TestArguments), TestRuns (TestRuns),
                     doBuild)
import           CommonBuild (CoverallsConfig (CoverallsConfig),
                     CoverallsTokenEnvVar (CoverallsTokenEnvVar),
                     ExtraShcArgs (ExtraShcArgs), IO,
                     TixDirectory (TixDirectory))


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
