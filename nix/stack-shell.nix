# This is the derivation used by "stack --nix".
# It provides the system dependencies required for a stack build.
{ decentralizedUpdatesPackages ? import ../nix {}
, pkgs ? decentralizedUpdatesPackages.pkgs
}:
with pkgs;

haskell.lib.buildStackProject {
  name = "decentralized-updates-stack-env";
  ghc =  (import ../shell.nix {inherit pkgs;}).ghc;

  buildInputs =
    # Development libraries which may be necessary for the build.
    # Add remove libraries as necessary:
    [ zlib gmp openssl git systemd.dev ] ++
    # MacOS-specific librararies which may be necessary for the build.
    (lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [ Cocoa CoreServices libcxx libiconv ])) ++
    # Add any programs/tools which are required for e.g. running tests.
    (with decentralizedUpdatesPackages; [ /* You can put programs here */ ]);

  phases = ["nobuildPhase"];
  nobuildPhase = "mkdir -p $out";
}
