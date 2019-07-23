{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
# Import IOHK common nix lib
, iohkLib ? import ./nix/iohk-common.nix { inherit system crossSystem config; }
# Use nixpkgs pin from iohkLib
, pkgs ? iohkLib.pkgs
}:

with import ./nix/util.nix { inherit pkgs; };

let
  haskell = iohkLib.nix-tools.haskell { inherit pkgs; };
  src = iohkLib.cleanSourceHaskell ./.;

  # You can declare packages from from iohk-nix required by the build. Example:
  #
  # inherit (iohkLib.rust-packages.pkgs) jormungandr;

  # Import the Haskell package set.
  haskellPackages = import ./nix/default.nix {
    inherit pkgs haskell src;
    # Pass in any extra programs necessary for the build as function arguments.
    # Here you candeclare packages required by the build, e.g.:
    # inherit jormungandr;
    # inherit (pkgs) cowsay;
    # Provide cross-compiling secret sauce
    inherit (iohkLib.nix-tools) iohk-extras iohk-module;
  };

in {
  inherit pkgs iohkLib src haskellPackages;
  inherit (haskellPackages.decentralized-updates.identifier) version;

  # Grab the executable component of our package.
  inherit (haskellPackages.decentralized-updates.components.exes)
    decentralized-updates;

  tests = collectComponents "tests" isDecentralizedUpdates haskellPackages;
  benchmarks = collectComponents "benchmarks" isDecentralizedUpdates haskellPackages;

  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = haskellPackages.shellFor {
    name = "decentralized-updates-shell";
    # List all local packages in the project.
    packages = ps: with ps; [
      decentralized-updates
    ];
    # These programs will be available inside the nix-shell.
    buildInputs =
      with pkgs.haskellPackages; [ hlint stylish-haskell weeder ghcid lentil ]
      # You can add your own packages to the shell.
      ++ [  ];
  };

  # If you need to run CI check scripts, you could use something like:
  #
  # checks.lint-fuzz = pkgs.callPackage ./nix/check-lint-fuzz.nix {};
  #
  # See `skeleton` project at https://github.com/input-output-hk/iohk-nix/
  #

  docs = import ./docs/default.nix { inherit pkgs; };
}
