{ pkgs

# haskell.nix
, haskell

# Filtered sources of this project
, src

# Example: test dependencies of decentralized-updates
# , jormungandr
# , cowsay

# Customisations for cross-compiling
, iohk-extras ? {}
, iohk-module ? {}

}:

let
  # our packages
  stack-pkgs = import ./.stack.nix/default.nix;

  # Grab the compiler name from stack-to-nix output.
  compiler = (stack-pkgs.extras {}).compiler.nix-name;

  pkgSet = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    modules = [
      # Add source filtering to local packages
      {
        packages.decentralized-updates.src = src;
        # packages.another-package = src + /another-package;
      }

      # Add dependencies
      {
        packages.decentralized-updates.components.tests = {
          unit.build-tools = [ jormungandr ];
        };

        # How to set environment variables for builds
        # packages.decentralized-updates.preBuild = "export NETWORK=testnet";

        # fixme: Workaround for https://github.com/input-output-hk/haskell.nix/issues/207
        packages.decentralized-updates.components.all.postInstall = pkgs.lib.mkForce "";
      }

      # Misc. build fixes for dependencies
      {
        # Cut down iohk-monitoring deps
        packages.iohk-monitoring.flags = {
          disable-ekg = true;
          disable-examples = true;
          disable-graylog = true;
          disable-gui = true;
          disable-prometheus = true;
          disable-systemd = true;
        };

        # Katip has Win32 (>=2.3 && <2.6) constraint
        packages.katip.doExactConfig = true;
      }

      # the iohk-module will supply us with the necessary
      # cross compilation plumbing to make Template Haskell
      # work when cross compiling.  For now we need to
      # list the packages that require template haskell
      # explicity here.
      iohk-module
    ];
    pkg-def-extras = [
      iohk-extras.${compiler}
      (hackage: { packages = {
        "transformers" = (((hackage.transformers)."0.5.6.2").revisions).default;
        "process" = (((hackage.process)."1.6.5.0").revisions).default;
      }; })
    ];
  };

in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
