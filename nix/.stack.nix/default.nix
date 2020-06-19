{
  extras = hackage:
    {
      packages = {
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "hedgehog" = (((hackage.hedgehog)."1.0").revisions).default;
        "micro-recursion-schemes" = (((hackage.micro-recursion-schemes)."5.0.2.2").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.3.0.1").revisions).default;
        "cborg" = (((hackage.cborg)."0.2.2.0").revisions).default;
        "canonical-json" = (((hackage.canonical-json)."0.6.0.0").revisions).default;
        "terminal-progress-bar" = (((hackage.terminal-progress-bar)."0.4.1").revisions)."ba857f3424ddb1034125163a9a384e9baab22e55de968259b046892c20ec0526";
        "monad-validate" = (((hackage.monad-validate)."1.2.0.0").revisions)."9850f408431098b28806dd464b6825a88a0b56c84f380d7fe0454c1df9d6f881";
        "Unique" = (((hackage.Unique)."0.4.7.7").revisions)."2269d3528271e25d34542e7c24a4e541e27ec33460e1ea00845da95b82eec6fa";
        decentralized-updates = ./decentralized-updates.nix;
        datil = ./datil.nix;
        cardano-prelude = ./cardano-prelude.nix;
        cardano-binary = ./cardano-binary.nix;
        cardano-crypto-class = ./cardano-crypto-class.nix;
        cardano-slotting = ./cardano-slotting.nix;
        successors = ./successors.nix;
        cardano-crypto = ./cardano-crypto.nix;
        };
      compiler.version = "8.6.5";
      compiler.nix-name = "ghc865";
      };
  resolver = "lts-13.26";
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "decentralized-updates" = {
            flags = {
              "asserts" = lib.mkOverride 900 true;
              "pretty" = lib.mkOverride 900 true;
              };
            };
          };
        })
    { packages = {}; }
    ];
  compiler = "ghc-8.6.5";
  }