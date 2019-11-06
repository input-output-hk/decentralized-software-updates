let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
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
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."generic-monoid" or (buildDepError "generic-monoid"))
          (hsPkgs."bimap" or (buildDepError "bimap"))
          (hsPkgs."hedgehog" or (buildDepError "hedgehog"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."hashable" or (buildDepError "hashable"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."cardano-binary" or (buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (buildDepError "cardano-crypto-class"))
          (hsPkgs."small-steps" or (buildDepError "small-steps"))
          (hsPkgs."cs-ledger" or (buildDepError "cs-ledger"))
          ];
        buildable = true;
        };
      tests = {
        "ledger-rules-test" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."hedgehog" or (buildDepError "hedgehog"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."tasty-hedgehog" or (buildDepError "tasty-hedgehog"))
            (hsPkgs."cardano-crypto-class" or (buildDepError "cardano-crypto-class"))
            (hsPkgs."decentralized-updates" or (buildDepError "decentralized-updates"))
            (hsPkgs."cs-ledger" or (buildDepError "cs-ledger"))
            (hsPkgs."small-steps" or (buildDepError "small-steps"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././executable-spec; }