{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "small-steps"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "https://github.com/input-output-hk/cardano-chain";
      url = "";
      synopsis = "Small step semantics";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.free)
          (hsPkgs.goblins)
          (hsPkgs.hedgehog)
          (hsPkgs.tasty-hunit)
          (hsPkgs.lens)
          (hsPkgs.mtl)
          (hsPkgs.transformers)
          (hsPkgs.cardano-crypto-class)
          ];
        };
      tests = {
        "doctests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.containers)
            (hsPkgs.data-default)
            (hsPkgs.free)
            (hsPkgs.hedgehog)
            (hsPkgs.tasty-hunit)
            (hsPkgs.lens)
            (hsPkgs.mtl)
            (hsPkgs.sequence)
            (hsPkgs.transformers)
            (hsPkgs.doctest)
            (hsPkgs.small-steps)
            ];
          build-tools = [
            (hsPkgs.buildPackages.doctest-discover or (pkgs.buildPackages.doctest-discover))
            ];
          };
        "examples" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.containers)
            (hsPkgs.hedgehog)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hedgehog)
            (hsPkgs.tasty-expected-failure)
            (hsPkgs.small-steps)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger-specs";
      rev = "d474b630d67d6548172874cfe7a1c87e3c616927";
      sha256 = "0z997sj4d8xj338gb1rls5mbvd7pz73h2zpvgm760f4nw2g6bqk4";
      });
    postUnpack = "sourceRoot+=/byron/semantics/executable-spec; echo source root reset to \$sourceRoot";
    }