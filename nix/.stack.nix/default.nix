{
  extras = hackage:
    {
      packages = {
        "tasty-hedgehog" = (((hackage.tasty-hedgehog)."1.0.0.1").revisions).default;
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "hedgehog" = (((hackage.hedgehog)."1.0").revisions).default;
        "micro-recursion-schemes" = (((hackage.micro-recursion-schemes)."5.0.2.2").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.3.0.1").revisions).default;
        "bimap" = (((hackage.bimap)."0.4.0").revisions)."d56712c094d40b4666e30d35fb0dad4058a2b753c5af4574f062237b2f7d5b67";
        } // {
        decentralized-updates = ./decentralized-updates.nix;
        small-steps = ./small-steps.nix;
        cs-ledger = ./cs-ledger.nix;
        cborg = ./cborg.nix;
        cardano-crypto = ./cardano-crypto.nix;
        canonical-json = ./canonical-json.nix;
        };
      compiler.version = "8.6.5";
      compiler.nix-name = "ghc865";
      };
  resolver = "lts-13.26";
  compiler = "ghc-8.6.5";
  }