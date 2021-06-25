This directory contains an implementation of the decentralized system updates
mechanism for stake-based blockchains described in the [design
specification](../design-spec). This implementation was materialized as a
Haskell library that was designed to be plugged in the [ledger
layer](https://github.com/input-output-hk/cardano-ledger-specs) of Cardano.

Module `Cardano.Ledger.Update` contains the API that the ledger can use. This
API is divided into state update and state query functions.

State update functions take as parameters a combination (of some) of the
following values:

- an environment,
- an update payload, and
- an update system state.

The state update functions are `initialState`, `tick`, and `apply`.

The API is polymorphic on the type of environments. The `*.Env.*` namespace
contains the classes that model restrictions that the update module imposes on
the environment instances.

Similarly, the API does not constrain the proposals to a specific type, but it
defines a `Proposal` class which provides a generic model for proposals.
Module `Cardano.Ledger.Update.Proposal` contains the definition of this
class. Its instances define what:

- submissions,
- revelations,
- vote, and
- voters

are. Moreover, we abstracted away:

- signatures,
- commits, and
- hashes.

These abstractions can be found in the `Cardano.Ledger.Update.Proposal` module.
More specifically:

- the `Signed` class abstracts away data whose signature can be verified. In
  practice, verifying signatures on the data submitted in a transaction is
  responsibility of the ledger layer that uses this API, so this class and its
  usages should be removed.
- The `Commitable` class introduces the concept of data for which a commit can
  be calculated. This makes it possible to make use of the commit-reveal
  protocol when submitting proposals.
- The `Identifiable` class introduces the concept of data for which an unique id
  can be calculated. In practice, the calculation of such an id will realized by
  means of a `hash` function. In the tests of this library, this calculation was
  realized by unsigned integers instead, which made the execution of the
  property tests considerably faster.

Some proposals take the form of protocol updates, and can therefore be
activated. Class `Activable` in `Cardano.Ledger.Update.Proposal` models the
concept of protocol updates that can be activated. In particular, instances of
this class define the endorsers and protocol version types.

State query functions allow to retrieve information about update proposals and
current protocol without breaking the encapsulation of the update API.

## Plugging delegation

The stake distribution that some API functions require is provided as a
parameter, inside the environment. This makes the delegation to experts an
orthogonal concern: the update protocol does not care how the stake distribution
is elaborated. This would be the responsibility of a component that deals with
delegation to experts. In this way, the current implementation allows for its
use in combination with a delegation mechanism.

## Module structure

The module structure of the implementation is shown below.

![Implementation module structure](images/src-intra-deps.svg)

[... look how nice: activation depends on approval which in turn depends on ideation ...]

...

... # tests

... we have unit tests

... explain what are traces

... explain how to get examples of traces ...

... features of our testing framework
... - simple generators ... where can the generators be found?

[... # bring the testing framework presentation back here OR link to the .org file here ]

... things worth mentioning from the presentation
  ... system under test
  ... scenario

... explain the liveness tests

... explain the state change validity tests

... explain the coverage tests

... mention tests module structure

... # bechmarks

... # works case analysis
