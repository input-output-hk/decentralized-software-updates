{ pkgs ? import ../nix {}
, haskellBuildUtils ? pkgs.haskellBuildUtils
}:

haskellBuildUtils.stackRebuild {
  script = ./rebuild.hs;
  buildTools = [];
  libs = ps: [];
  shell = import ../nix/stack-shell.nix;
}
