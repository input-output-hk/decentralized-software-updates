<h1 align="center">On decentralized software updates for blockchain systems</h1>

<p align="center">
  <a href="https://buildkite.com/input-output-hk/decentralized-software-updates">
    <img alt="Build Status" src="https://img.shields.io/buildkite/0400e4f455135087dc9f04095439712f6c0225226e9d2ab623/master?style=for-the-badge"/>
  </a>
  <a href="https://coveralls.io/github/input-output-hk/decentralized-software-updates?branch=master">
    <img alt="Coverage Status" src="https://img.shields.io/coveralls/github/input-output-hk/decentralized-software-updates?style=for-the-badge"/>
  </a>
</p>

This repository contains research output (papers, specifications, models, and
executable software) about decentralized software updates for blockchain
systems. Our research focuses on defining a protocol that covers the life cycle
of software updates, which consists of:

- ideation: the definition and specification of an update proposal, similar to
  [Bitcoin](https://github.com/bitcoin/bips) or
  [Ethereum](https://github.com/ethereum/EIPs) improvement proposals.
- implementation: the actual implementation of improvement proposals.
- activation: the activation of new software version across nodes in the
  blockchain.


This work is funded by the [Priviledge](https://priviledge-project.eu/) project,
and these results will be incorporated into the
[Voltaire](https://cardanoroadmap.com/en/voltaire/) release of Cardano.

The research roadmap can be found in [our
wiki](https://github.com/input-output-hk/decentralized-software-updates/wiki/Roadmap).

## Design specification

The design specification is written in `LaTeX`. It can be found in the
[`design-spec`](./design-spec) folder. A compiled pdf can be found
[here](https://hydra.iohk.io/job/Cardano/decentralized-software-updates/specs.decentralized-updates/latest/download-by-type/doc-pdf/priviledge-decentralized-swupdates).

We use [`nix`](https://nixos.org/nix/download.html) to achieve not only
reproducible software builds, but also reproducible document builds across
developers machines. This means that anybody who clones this code should be able
to build the software and documents without requiring any additional setup other
than having `nix` installed. But of course, the use of `nix` is not required.

When using `nix` it is recommended that you setup the cache, so that it can
reuse built artifacts, reducing the compilation times dramatically:

If you are using [NixOS](https://nixos.org/) add the snippet below to your
`/etc/nixos/configuration.nix`:

```
nix.binaryCaches = [
  "https://cache.nixos.org"
  "https://hydra.iohk.io"
];

nix.binaryCachePublicKeys = [
  "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
];
```

If you are using the `nix` package manager next to another operating system put
the following in `/etc/nix/nix.conf` if you have a system-wide `nix`
installation , or in `~/.config/nix/nix.conf` if you have a local installation:

```
substituters        = https://hydra.iohk.io https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

The PDF corresponding to the formal specification can be built by running:

```sh
nix-build -A specs -o spec.decentralized-updates
```

The above command will create a `spec.decentralized-updates` folder that
contains the compiled document.

To get a shell where you have access to all the necessary tools for building the
document run:

```sh
nix-shell default.nix -A specs.decentralized-updates
```

Once in the `nix-shell` the document can be built by running `make` inside the
`design-spec` directory. When editing the document it is also useful to
recompile on file changes. To this end use `make watch`.

## Executable specification

The executable specifications for the update mechanism can be found in the
[`executable-spec`](executable-spec/) folder. Currently we only support building
using `nix` because this is the build method used at IOHK.

To test the executable specifications enter a `nix` shell use `cabal`:

```sh
nix-shell
cabal test all
```

Alternatively you can setup [`lorri`](https://github.com/target/lorri) so that
cabal is available without needing to enter the nix shell.

## Contributing

Make sure:

- your editor supports [`editorconfig`](https://editorconfig.org/).
- you have the latest version of
  [`stylish-haskell`](https://github.com/jaspervdj/stylish-haskell/) installed
  (we use features that are not in Hackage yet, so make sure to build from
  source).
- Make sure your editor runs `stylish-haskell` on save.

We try to adhere to the coding standard explained
[here](https://github.com/input-output-hk/cardano-wallet/wiki/Coding-Standards)

<hr/>

<p align="center">
  <a href="https://github.com/input-output-hk/decentralized-software-updates/blob/master/LICENSE">
    <img src="https://img.shields.io/github/license/input-output-hk/cardano-wallet.svg?style=for-the-badge"/>
  </a>
</p>
