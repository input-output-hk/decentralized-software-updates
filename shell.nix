# This file is used by nix-shell.
{ config ? {}
, sourcesOverride ? {}
, withHoogle ? true
, pkgs ? import ./nix {
    inherit config sourcesOverride;
  }
, sources ? import ./nix/sources.nix
, stack2nix ? sources.stack2nix
}:
with pkgs;
let
  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = decentralizedUpdatesHaskellPackages.shellFor {
    name = "dsu-dev-shell";

    # If shellFor local packages selection is wrong,
    # then list all local packages then include source-repository-package that cabal complains about:
    packages = ps: with ps; [
       decentralized-updates
       datil
       cardano-slotting
    ];

    # These programs will be available inside the nix-shell.
    buildInputs = with haskellPackages; [
      cabal-install        # The command-line interface for Cabal and Hackage
      ghcid                # GHCi based bare bones IDE
      git                  # Distributed version control system
      hlint                # Source code suggestions
      nix                  # Purely Functional Package Manager
      niv                  # Dependency management for Nix projects
      pkgconfig            # Allows packages to find out information about other packages
      sqlite-interactive   # Self-contained, serverless, zero-configuration, transactional SQL database
      stack                # The Haskell Tool Stack
      stack2nix            # Convert stack.yaml files into Nix build instructions.
      tmux                 # Terminal multiplexer
      weeder               # Detect dead Haskell code
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;

    inherit withHoogle;
    shellHook = ''
      echo "DSU Tools" \
      | ${figlet}/bin/figlet -f banner -c \
      | ${lolcat}/bin/lolcat

      echo "NOTE: you may need to export GITHUB_TOKEN if you hit rate limits with niv"
      echo "Commands:
        * niv update <package> - update package
        * stack --nix build - use nix to direct the stack build

      "
    '';
  };

  devops = pkgs.stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = [
      niv         # Dependency management for Nix projects
      stack       # The Haskell Tool Stack
      stack2nix   # Convert stack.yaml files into Nix build instructions.
    ];
    shellHook = ''
      echo "DevOps Tools" \
      | ${figlet}/bin/figlet -f banner -c \
      | ${lolcat}/bin/lolcat

      echo "NOTE: you may need to export GITHUB_TOKEN if you hit rate limits with niv"
      echo "Commands:
        * niv update <package> - update package
        * stack --nix build - use nix to direct the stack build

      "
    '';
  };

in

 shell // { inherit devops; }
