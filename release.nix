############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################

# The project sources
{ decentralized-updates ? { outPath = ./.; rev = "abcdef"; }

# Function arguments to pass to the project
, projectArgs ? {
    config = { allowUnfree = false; inHydra = true; };
    gitrev = decentralized-updates.rev;
  }

# The systems that the jobset will be built for.
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]

# The systems used for cross-compiling
, supportedCrossSystems ? [ "x86_64-linux" ]

# A Hydra option
, scrubJobs ? true

# Dependencies overrides
, sourcesOverride ? {}

# Import pkgs, including IOHK common nix lib
, pkgs ? import ./nix { inherit sourcesOverride; }

}:

with (import pkgs.iohkNix.release-lib) {
  inherit pkgs;
  inherit supportedSystems supportedCrossSystems scrubJobs projectArgs;
  packageSet = import decentralized-updates;
  gitrev = decentralized-updates.rev;
};

with pkgs.lib;

let
  testsSupportedSystems = [ "x86_64-linux" ];
  collectTests = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);

  inherit (systems.examples) musl64;

  jobs = {
    native = mapTestOn (packagePlatforms project);
  } // (
    # This aggregate job is what IOHK Hydra uses to update
    # the CI status in GitHub.
    mkRequiredJob (
      collectTests jobs.native.tests ++
      collectTests jobs.native.benchmarks ++
      # Add your project executables to this list if any:
      [ # jobs.native.decentralized-updates.x86_64-linux
      ]
    )
  )
  # Collect all spec PDFs, without system suffix
  // { inherit (project)
         decentralizedUpdatesSpec;
     }
  # Build the shell derivation in Hydra so that all its dependencies
  # are cached.
  // mapTestOn (packagePlatforms { inherit (project) shell; });

in jobs
