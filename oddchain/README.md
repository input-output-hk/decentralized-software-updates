# About

This project defines a simple ledger layer, and integrates it to the Cardano
[consensus and network
layers](https://github.com/input-output-hk/ouroboros-network) so that a node can
be run. This project is intended to serve as a reference for those who want to
integrate their own ledgers to the reusable parts of Cardano.

We present next a high level description of the ledger logic that this project
implements, how to run a demo, and how the code is organized.

# The Oddchain

We define a ledger layer for an _oddchain_: a chain that consists of odd numbers
only. Blocks of the _oddchain_ consists of a sequence of odd numbers that are
strictly increasing of decreasing, according to the current _phase_. The _phase_
determines:

- Whether the numbers inside a block should decrease or increase
- A bound for the numbers in the block. If the phase is _decreasing_ then the
  phase determines the upper bound, if the phase is increasing then the phase
  determines the lower bound.

For instance, if the current phase is decreasing and has a bound of 10, then a
block produced in that phase must contain decreasing sequences of odd numbers
smaller than 10.

The bound of a phase changes after each block, and the sign of the phase
(decreasing or increasing) is flipped after each epoch.

## Running the demo

The demo can run multiple nodes in a local machine. It shouldn't be too hard to
modify this project to support running nodes in different machines, but for the
sake of simplicity we do not implement this functionality.

This project has been tested on Linux only at the moment. If you have access to
a machine with a different OS feel free to try this out as well, and submit a PR
with the changes needed to run this in different platforms :heart:.

If you have [`stack`](https://docs.haskellstack.org/en/stable/README/) and
[`tmux`](https://github.com/tmux/tmux/wiki) installed and use a Unix based
operating system, we provide a simple script to start multiple nodes and
clients.

First make sure that you run:

```sh
stack build
```

After that, inside a `tmux` session run:

```sh
./oddchain/demo.hs --numNodes 3
```

The number of nodes can be between 1 and 3. If you have enough screen real state
you could try modifying the script to allow a larger number of nodes.

This will start `numNodes` nodes and `numNodes` clients (wallets). The nodes
communicate to each other and issue blocks according to the Byzantine Fault
Tolerant (BFT) consensus protocol, whereas the clients submit random
transactions to their corresponding nodes.

What you should see is a terminal split in `numNodes * 2` panes, where the top
layer shows the nodes' output, and the bottom layer shows the clients' (simple
wallet) output. At this bottom layer you should see the blocks produced being
reported, and of course, all the clients should agree on the blocks they see
:slightly_smiling_face:.

### Running nodes individually

If you wish to run nodes individually, this can be done as follows:

```sh
stack exec oddnode -- \
--numberOfCoreNodes 3 \
--nodeId 2 \
--enableTracers Consensus \
--startTimeString '2020-03-18 11:27:32.925865787 UTC'
```

here:

- `numberOfCoreNodes` determines how many core nodes are in the network.
- `nodeId` specifies the node id, which must be between 0 and `numberOfCoreNodes - 1`.
- `enableTracers` allows to enable different tracers. See `OddNode.NodeTracer`
  for available options.
- `startTimeString` specifies the time at which the blockchain started. It is
  **crucial** that all nodes in the network have the same start time.


To run a client (wallet) that submits transaction to its corresponding node you
can use:

```sh
stack run oddwallet -- --nodeId 2 --enabledTracers Rolls
```

where the `nodeId` and `enabledTracers` parameters have the same meaning above
described. The data type `OddWallet.WalletTracer` defines the available tracers
for the wallet.

## Structure of the project

Directory `app` contain the main files for the node and wallet.

The entry point for running a node can be found in `src/OddNode.oddRun`. See the
docstring in this function for some caveats that apply to this demo.

Function `oddRun` requires several
[`ouroboros-consensus`](https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-consensus)
instances, which are defined in `module OddChain`. The major instances one has to define are:

- `BlockProtocol` type instance. This determines the protocol that the blocks
  support.
- `BlockSupportsProtocol` class instance. This determines how to obtain the data
  required to validate the given header according to the consensus rules
  (`ValidateView`) and how to obtain the data required to select among competing
  chains (`SelectView`).
- `LedgerSupportsProtocol` class instance. This links the ledger to the
  protocol, by having the former giving the necessary information to the latter.
  The consensus algorithm uses this information to determine if the node should
  produce a block (`LedgerView`). For the BFT protocol, the ledger view is just
  `()`, since leader election is based on a round-robin schedule. More
  sophisticated consensus protocols, such as
  [Praos](https://iohk.io/en/research/library/papers/ouroboros-praosan-adaptively-securesemi-synchronous-proof-of-stake-protocol/),
  might require other information like some approximation of the stake
  distribution.
- `UpdateLedger` class instance. This defines how blocks can be applied to the
  ledger state to update it. This is where the ledger logic lives.
- `ApplyTx` class instance. Similar to `UpdateLedger`, this instance defines how
  to different transactions affect the ledger state. This instance is used by
  the mempool. Property `Test.OddChain.prop_mempool` defines the relation that
  is expected to hold between the instances of these type classes. Be aware that
  there might be other properties that aren't currently captured in the tests.
- `RunNode` instance. This defines, among other things, functions to:
  - forge blocks when the node is the leader (given, among other things, a
    sequence of transactions)
  - determine if the header matches the body
  - serialize and deserialize data (blocks, headers, transactions, etc)

In addition, the consensus layer does not prescribe which encoding to use. We
use [`CBOR`](https://tools.ietf.org/html/rfc7049) for this purpose, and the
`ToCBOR`/`FromCBOR` instances are defined in the `OddChain` module as well.

The module `OddChain` defines `HasHeader` instances for both odd blocks and odd
block headers. This is required since at certain stages the consensus layer
would have either the whole block available or only the headers.

Besides these instances, the `OddChain` module contains a definition of the
block type for the odd chain, together with a definition of the ledger state.
When defining the ledger state is **important** that that the ledger state can
retrieve the header hash of the last applied block (see the instance of function
`ledgerTipPoint`).

The `OddWallet` module contains the implementation of a simple wallet that
submits random transactions to the node it is connected to.

Tests for the implementation are defined in the following modules:

- `Test.ThreadNet.OddChain` which contains:
  - a test to check that the consensus layer is behaving as expected for the odd
    chain instance (see `prop_simple_oddchain_convergence`)

- `Test.OddChain` which contains:
  - roundtrip tests to make sure serialization and deserialization work as
    expected.
  - header encoding tests, to make sure that we correctly implement
    `nodeEncodeBlockWithInfo`.
  - ledger property tests (see next section).

## Properties that the ledger layer should satisfy

### Relation between `UpdateLedger` and `ApplyTx` instances

Applying a sequence of transactions should result in the same state as applying
those transactions as part of a block's payload. See
`Test.OddChain.prop_mempool`.

### Re-applying succeeds if applying does

Function `reapplyLedgerBlock` should skip block payload validation but update
the ledger state. So this means that if `applyLedgerBlock` succeeded, then
`reapplyLedgerBlock` should succeed as well and they should end up in the same
state.

Functions `applyTx` and `reapplyTx` should also satisfy this property.

### Lemmas

The following lemmas are checked by consensus when assertions are enabled. See
`flag asserts`. See also [this issue]().

#### Relation between `anachronisticProtocolLedgerView` and  `applyChainTick`

The anachronistic protocol ledger view returns the same view as the one obtained
by applying chain tick with the given slot. Formally:

```
for all ledgerTipSlot st <= s <= L ,
     anachronisticProtocolLedgerView cfg st s
  == protocolLedgerView (applyChainTick cfg s st)
```

where `L` is the maximum number of slots the ledger can look ahead (for instance
for Byron this is `2k`).

#### Relation between block application and chain tick

Applying a block and applying the chain tick result in the same protocol ledger
view. Formally:


```
     protocolLedgerView (applyLedgerBlock cfg blk state)
  == protocolLedgerView (applyChainTick cfg (blockSlot blk) state)
```

### Relation between applying a block, chain ticks, and applying transactions

Applying a **valid** block `blk` on state `st` should result in the same state
as ticking the ledger state by the block slot, and then applying the
transactions inside the block. Note that here **validity** of block is crucial.
If the block is invalid in the given state, then we can't say anything about the
equality. Formally, for all valid blocks:

```
   applyLedgerBlock cfg blk st
 == repeatedlyM (applyTx cfg) (extractTxs blk)
                (applyChainTick cfg (blockSlot blk) st)
```
