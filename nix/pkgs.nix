# our packages overlay
pkgs: _: with pkgs; {
  decentralizedUpdatesHaskellPackages = import ./haskell.nix {
    inherit config
      lib
      stdenv
      haskell-nix
      buildPackages
      ;
  };
}
