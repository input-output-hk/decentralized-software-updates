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

## Formal specification

The formal specification is written in `LaTeX`. It can be found in the
[`formal-spec`](./formal-spec) folder. A compiled pdf can be found
[here](https://hydra.iohk.io/job/Cardano/decentralized-software-updates/decentralizedUpdatesSpec/latest/download-by-type/doc-pdf/decentralized-updates).

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

To get a shell where you have access to all the necessary tools for building the
document run:

```sh
nix-shell default.nix -A specs.decentralized-updates
```

Once in the `nix-shell` the document can be built by using `make`. When editing
the document it is also useful to recompile on file changes. To this end, use
`make watch`.

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

## Integration with Cardano

The update protocol implemented in this repository was integrated into Cardano,
although this was not merged into the `master` branch of any of the its
components, due to the experimental nature of this work. However, as a result of
this integration the ledger layer of Cardano was made parametric in the update
protocol. This was a significant change that did get merged into the `master`
branch of
[`cardano-ledger-specs`](https://github.com/input-output-hk/cardano-ledger-specs).

The update protocol integration with Cardano took place across several
components:

- Ledger.
- Consensus.
- Node and Cardano command line interface.
- Devops infrastructure.

The [`cardano-node`](https://github.com/input-output-hk/cardano-node/)
repository, commit
[61e44a22f73969054070edb4668b5b922de94107](https://github.com/input-output-hk/cardano-node/commit/61e44a22f73969054070edb4668b5b922de94107),
contains the references to all the components and their corresponding commits in
the `cabal.project` file:

```cabal
source-repository-package
  type: git
  location: https://github.com/input-output-hk/decentralized-software-updates
  tag: e8325a440782a9c362a121340cea64a37e876e2a
-- ...

source-repository-package
  type: git
  location: https://github.com/input-output-hk/cardano-ledger-specs
  tag: 3626206b45e68499d7869a4aeeac6ed5889a490d
-- ...

source-repository-package
  type: git
  location: https://github.com/input-output-hk/ouroboros-network
  tag: 035e65bdfc2d79ccd4a204e1735c16e8307991cb
-- ...

```

The repository where the DevOps infrastructure repository
[`cardano-ops`](https://github.com/input-output-hk/cardano-ops), in commit
[5c630ba534d0211a12f3a98fb8796b630b7a56e1](https://github.com/input-output-hk/cardano-ops/commit/5c630ba534d0211a12f3a98fb8796b630b7a56e1)
contains a script for testing the integration, and a script for benchmarking a
run of the update protocol. Both scripts require a system with `nix` installed.
We mention how to run them later on.

The integration testing script setups a testnet consisting of OBFT nodes that
produce blocks, and pool nodes which register stake pools and participate in the
update protocol.

In this test, an update proposal to double the maximum block size is activated,
after going through the ideation and approval phases. The following steps take
place:

0. Initially each pool node registers a stake pool. This is required since only
   active stake, i.e., stake delegated to stake pools, is considered in the
   stake distribution snapshot that the ledger computes and our update mechanism
   uses.
1. An SIP is submitted by one of the pool nodes.
2. All the pool nodes vote for the SIP.
3. After the SIP is approved, one of the pool nodes submits the implementation.
4. All the pool nodes vote for the implementation.
5. After the implementation is approved, all the pool nodes endorse it.
6. Once the proposal is endorsed, at an epoch change the update is activated.

In parallel with the steps above, two processes are run:

0. A process that submits a random amount of Lovelace to newly created keys. The
   goal of this process is to show that other transactions can take place
   during and after an update.
1. A process that monitors the update state of the ledger, printing the results
   on screen. This process terminates when the protocol version changes to the
   protocol version of the update proposal submitted in this script.

A recording of the demo can be found [here](https://drive.google.com/file/d/1jVNL7NXsvR5OpshtQqU1j9SfGA7tL0GK/view?usp=sharing "Video of a demo run of the prototype").

To run this demo check out `cardano-ops`, and inside the root directory of this
project start a [`tmux`](https://github.com/tmux/tmux/wiki) session and run:

```sh
./examples/priviledge-demo.sh redeploy
```

In the `cardano-ops` repository (commit
`5c630ba534d0211a12f3a98fb8796b630b7a56e1`) the number of nodes can be changed by
editing the lists `bftNodeRegionNames` and `poolRegionNames` in
`topologies/pivo.nix`. This repository also:

- provides additional details on how to manipulate the testnet infrastructure
  read `examples/shelley-testnet/README.md`, in the `cardano-ops` repository.
- explains how the tesnet can be run on an AWS cluster, also in
  `examples/shelley-testnet/README.md`.

The `examples/pivo-version-change/aws-integration-test.patch` patch contains an
example of the changes necessary to run the integration tests with 10 pool nodes
on AWS. Besides modifying the `poolRegionNames` this patch delays blockchain
start time so that it takes place after all the nodes are deployed.

The benchmarking script implements the same logic as the test script, but it
uses a larger number of transaction submission threads and voting keys. In the
test script we used 10 voting keys, in the benchmarks we used a maximum of 12K
voting keys instead. Also different network parameters are used so that the
network can accommodate the additional load.

The benchmarks were run on AWS only because a large number of nodes needed to be
deployed. To run on AWS, the
`examples/pivo-version-change/aws-benchmarks.patch` must be applied. This
patch sets a different set of network parameters for the testnet w.r.t. the
integration tests, and uses a different number of nodes. Due to a quirk in the
AWS testnet setup, to guarantee that everything runs smoothly first the network
must be deployed and redeployed after some delay. To do this first enter the nix
shell:

```sh
nix-shell
```

and then run:

```sh
nixops destroy --confirm && nixops  deploy -k \
  && sleep 120; ./examples/pivo-benchmark.sh redeploy
```

The voting process can be followed by monitoring the script's logs:

```sh
tail -f voting-process.log
```

Upon completion the script copies the relevant logs from the nodes so that
they can be analyzed.

There is a program that processes the node logs and outputs (among other information):
- average UTxO transactions submitted during the voting period
- average latency during the voting period

The log analyzer program can be found in the `menl` directory, in the
`cardano-ops` repository. It expects the node logs to be in the root directory
of the `cardano-ops` repository. The log analyzer can be run using `stack` , and
can be executed as follows:

```sh
stack run
```

inside the `menl` directory. This program assumes that the following log files
are stored in the root directory of `cardano-ops`:
- `bft-node.log`
- `bft-nodes-tx-submission.log`
- `voting-timing.log`

These log files are produced by the benchmarking script. When run on an AWS
cluster, they should be copied to the machine where the logs will be analyzed.


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
