############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, stdenv
, pkgs
, haskell-nix
, buildPackages
, config ? {}
# GHC attribute name
, compiler ? config.haskellNix.compiler or "ghc8102"
# Enable profiling
, profiling ? config.haskellNix.profiling or false
}:
let

  src = haskell-nix.haskellLib.cleanGit {
      name = "decentralized-software-updates";
      src = ../.;
  };

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject {
    inherit src;
    compiler-nix-name = compiler;
    modules = [
      {
        packages.decentralized-software-updates.configureFlags =
          [ "--ghc-option=-Werror" ];
      }
    ];
  };
in
  pkgSet
