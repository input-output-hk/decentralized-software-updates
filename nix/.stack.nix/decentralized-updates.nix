{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "decentralized-updates"; version = "0.1.0.0"; };
      license = "LicenseRef-Apache";
      copyright = "2019 IOHK";
      maintainer = "formal.methods@iohk.io";
      author = "IO Research and IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "Decentralized updates for Cardano";
      description = "A prototype for a decentralized update mechanism for Cardano";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.containers)
          (hsPkgs.generic-monoid)
          (hsPkgs.bimap)
          (hsPkgs.hedgehog)
          (hsPkgs.text)
          (hsPkgs.hashable)
          (hsPkgs.cardano-prelude)
          (hsPkgs.small-steps)
          (hsPkgs.cs-ledger)
          ];
        };
      tests = {
        "ledger-rules-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.hedgehog)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-hedgehog)
            (hsPkgs.decentralized-updates)
            (hsPkgs.cs-ledger)
            (hsPkgs.small-steps)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././executable-spec; }
