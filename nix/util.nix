{ pkgs ? import <nixpkgs> {} }:

with pkgs.lib;

{
  # fixme: automate this in nix-tools/Haskell.nix.
  isDecentralizedUpdates = package:
    (hasPrefix "decentralized-updates" package.identifier.name);

  # fixme: upstream to iohk-nix
  collectComponents = group: packageSel: haskellPackages:
    (mapAttrs (_: package: package.components.${group} // { recurseForDerivations = true; })
     (filterAttrs (name: package: (package.isHaskell or false) && packageSel package) haskellPackages))
    // { recurseForDerivations = true; };
}
