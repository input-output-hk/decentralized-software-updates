{
  extras = hackage:
    {
      packages = {
        "Unique" = (((hackage.Unique)."0.4.7.6").revisions).default;
        "moo" = (((hackage.moo)."1.2").revisions).default;
        "gray-code" = (((hackage.gray-code)."0.3.1").revisions).default;
        "tasty-hedgehog" = (((hackage.tasty-hedgehog)."1.0.0.1").revisions).default;
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "hedgehog" = (((hackage.hedgehog)."1.0").revisions).default;
        "micro-recursion-schemes" = (((hackage.micro-recursion-schemes)."5.0.2.2").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.3.0.1").revisions).default;
        "cborg" = (((hackage.cborg)."0.2.2.0").revisions).default;
        "canonical-json" = (((hackage.canonical-json)."0.6.0.0").revisions).default;
        "bimap" = (((hackage.bimap)."0.4.0").revisions)."d56712c094d40b4666e30d35fb0dad4058a2b753c5af4574f062237b2f7d5b67";
        "generic-monoid" = (((hackage.generic-monoid)."0.1.0.0").revisions)."00d1a7b7ff0890a3e3cba4032be24fe377c26e5e45d73afcacd42c98abb3b7e3";
        "terminal-progress-bar" = (((hackage.terminal-progress-bar)."0.4.1").revisions)."ba857f3424ddb1034125163a9a384e9baab22e55de968259b046892c20ec0526";
        decentralized-updates = ./decentralized-updates.nix;
        cardano-prelude = ./cardano-prelude.nix;
        cardano-binary = ./cardano-binary.nix;
        cardano-crypto-class = ./cardano-crypto-class.nix;
        small-steps = ./small-steps.nix;
        cs-ledger = ./cs-ledger.nix;
        goblins = ./goblins.nix;
        cardano-crypto = ./cardano-crypto.nix;
        };
      compiler.version = "8.6.5";
      compiler.nix-name = "ghc865";
      };
  resolver = "lts-13.26";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  compiler = "ghc-8.6.5";
  }