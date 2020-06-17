# This is the derivation used by "stack --nix".
# It provides the system dependencies required for a stack build.
with import ./. {};

haskell.lib.buildStackProject {
  name = "decentralised-software-updates-stack-env";
  ghc = (import ../shell.nix { inherit pkgs; }).ghc.baseGhc;

  buildInputs =
    # Development libraries which may be necessary for the build.
    # TODO: Add remove libraries as necessary
    [ zlib gmp ncurses lzma openssl git systemd.dev ] ++
    # Add any programs/tools which are required for e.g. running tests.
    (with haskellNixPackages; [ /* TODO: Put programs here */ ]);

  phases = ["nobuildPhase"];
  nobuildPhase = "mkdir -p $out";
}
