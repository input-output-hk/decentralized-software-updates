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
      identifier = { name = "oddchain"; version = "0.1.0.0"; };
      license = "LicenseRef-Apache";
      copyright = "2020 IOHK";
      maintainer = "formal.methods@iohk.io";
      author = "IO Research and IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "A blockchain consisting of odd numbers only";
      description = "An example of integrating custom ledger with ouroboros-consensus";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."binary" or (buildDepError "binary"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."cborg" or (buildDepError "cborg"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."fingertree" or (buildDepError "fingertree"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."serialise" or (buildDepError "serialise"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."cardano-binary" or (buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (buildDepError "cardano-slotting"))
          (hsPkgs."ouroboros-consensus" or (buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
          (hsPkgs."contra-tracer" or (buildDepError "contra-tracer"))
          (hsPkgs."iohk-monitoring" or (buildDepError "iohk-monitoring"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."network" or (buildDepError "network"))
          (hsPkgs."ouroboros-network-framework" or (buildDepError "ouroboros-network-framework"))
          (hsPkgs."network-mux" or (buildDepError "network-mux"))
          (hsPkgs."typed-protocols" or (buildDepError "typed-protocols"))
          (hsPkgs."pretty-simple" or (buildDepError "pretty-simple"))
          ];
        buildable = true;
        };
      exes = {
        "oddnode" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."optparse-generic" or (buildDepError "optparse-generic"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."oddchain" or (buildDepError "oddchain"))
            (hsPkgs."ouroboros-consensus" or (buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
            ];
          buildable = true;
          };
        "oddwallet" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."optparse-generic" or (buildDepError "optparse-generic"))
            (hsPkgs."oddchain" or (buildDepError "oddchain"))
            (hsPkgs."ouroboros-consensus" or (buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
            ];
          buildable = true;
          };
        };
      tests = {
        "odd-tests" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cborg" or (buildDepError "cborg"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."serialise" or (buildDepError "serialise"))
            (hsPkgs."base16-bytestring" or (buildDepError "base16-bytestring"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (buildDepError "tasty-quickcheck"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."cardano-binary" or (buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-class" or (buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-slotting" or (buildDepError "cardano-slotting"))
            (hsPkgs."oddchain" or (buildDepError "oddchain"))
            (hsPkgs."ouroboros-consensus" or (buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-consensus-test-infra" or (buildDepError "ouroboros-consensus-test-infra"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././oddchain; }