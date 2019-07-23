let
  commonLib = import ./lib.nix;
  default = import ./default.nix {};
  # Path of nix-tools jobs that we want to evict from release.nix:
  disabled = [
    # FIXME: those tests freeze on darwin hydra agents:
  ];
in
{ decentralized-updates ? { outPath = ./.; rev = "acdef"; }, ... }@args:
commonLib.pkgs.lib.mapAttrsRecursiveCond
(as: !(as ? "type" && as.type == "derivation"))
(path: v: if (builtins.elem path disabled) then null else v)
(commonLib.nix-tools.release-nix {
  package-set-path = ./nix/nix-tools.nix;
  _this = decentralized-updates;

  # packages from our stack.yaml or plan file (via nix/pkgs.nix) we
  # are interested in building on CI via nix-tools.
  packages = [ "decentralized-updates" ];

  # The required jobs that must pass for ci not to fail:
  required-name = "required";
  extraBuilds = {
    inherit (default) nixosTests;
  };
  required-name = "decentralized-software-updates-required-checks";
  required-targets = jobs: [
    # Tests
    jobs.nix-tools.tests.decentralized-updates.update-rules-test.x86_64-linux
    jobs.nix-tools.tests.decentralized-updates.update-rules-test.x86_64-darwin
    # windows cross compilation targets
    jobs.nix-tools.tests.x86_64-pc-mingw32-decentralized-updates.update-rules-test.x86_64-linux
    ];

} (builtins.removeAttrs args ["decentralized-software-updates"])
