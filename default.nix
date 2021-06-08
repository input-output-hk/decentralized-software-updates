{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of iohk-nix:
# nix build -f default.nix cardano-node --arg sourcesOverride '{
#   iohk-nix = ../iohk-nix;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with overlays (iohk-nix and our packages).
, pkgs ? import ./nix { inherit system crossSystem config sourcesOverride; }
, gitrev ? pkgs.iohkNix.commitIdFromGitRepoOrZero ./.git
}:
with pkgs; with commonLib;

let
  haskellPackages = recRecurseIntoAttrs
    # we are only interested in listing the project packages:
    (selectProjectPackages decentralizedUpdatesHaskellPackages);

  uploadCoverallsScript = pkgSet:
    let
      projectPkgs = selectProjectPackages pkgSet;
      projectCoverageReport = pkgSet.projectCoverageReport;
    in writeShellScriptBin "uploadCoveralls.sh" ''
      ${commonLib.hpc-coveralls}/bin/hpc-coveralls all \
        ${concatStringsSep "\n  " (mapAttrsToList (_: p: "--package-dir .${p.src.origSubDir} \\") projectPkgs)}
        --hpc-dir ${projectCoverageReport}/share/hpc/vanilla \
        --coverage-mode StrictlyFullLines \
        --repo-token=$COVERALLS_REPO_TOKEN
    '';

  self = {
    inherit haskellPackages check-hydra;

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    libs = collectComponents' "library" haskellPackages;

    exes = collectComponents' "exes" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;
    };

    inherit (commonLib) hpc-coveralls;
    uploadCoverallsScript = uploadCoverallsScript decentralizedUpdatesHaskellPackages;

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };

    roots = decentralizedUpdatesHaskellPackages.roots;

    #
    # PDF builds of LaTeX documentation.
    #
    # To get a shell where you can run pdflatex to build it yourself, use:
    #   nix-shell default.nix -A specs.decentralized-updates
    #
    # To build all specs locally with Nix:
    #  nix-build -A specs -o spec.decentralized-updates
    #
    specs = recurseIntoAttrs {
      decentralized-updates =
        pkgs.callPackage ./design-spec/default.nix {};
    };

  };

in
  self
