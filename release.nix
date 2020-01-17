{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, supportedCrossSystems ? [ "x86_64-linux" ]
, scrubJobs ? true
, decentralized-updates ? { outPath = ./.; rev = "abcdef"; }
, projectArgs ? { config = { allowUnfree = false; inHydra = true; }; }
, iohkLib ? import ./nix/iohk-common.nix {}
}:

with (import iohkLib.release-lib) {
  inherit (import ./nix/iohk-common.nix {}) pkgs;

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
