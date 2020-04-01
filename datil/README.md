# Data-Automata Testing and Interpretation Library

`datil` allows to define data automata models in Haskell, run them, and generate
traces of these models to test properties about them.

Datil is developed in the context of the work carried out by [IOHK
research](https://iohk.io/research/) for the [Priviledge
project](https://priviledge-project.eu/). The development of this library is
mainly driven by what said work demands. However, we welcome external
contributors that find this project helpful and want to improve this library
:heart:. See the [TODO](#todo) section.

# What can you do with `datil`

This library makes it possible to model all the possible sequences of events a
system allows using data automata. These models can be run, explored, and tested using

## The data automata formalism

Data automata are [automata](https://en.wikipedia.org/wiki/Automata_theory)
consisting of _states_, and transitions between states. Each transition has a
_guard_, which define the conditions under which actions can be triggered,
_actions_ that label the events of the system (and can be used for modeling
sending and receiving data), and an _update_ statement that defines how the
_variables_ associated to the automata are changed by the transition.

In addition, automata can be run in parallel, and they can define on which
actions they synchronize. See `module Control.State.DataAutomata` for more
details on the syntax and semantics of this formalism.

There are several examples in the [Examples](test/examples/Datil/Examples/)
folder which illustrate the use of data automata.

# TODO

The following is an unprioritized list of improvements to the `datil` library.

## Expressiveness

- [ ] Add support for richer expressions. Some examples of functions what can be
      added to the language include:
  - [ ] Union override for relations
  - [ ] Membership testing for relation

## Sanity checking models

- [ ] Check that all variables in the `RunnableModel.automata` are declared in
      `RunnableModel.initialMemory`. We could also check types as well.
- [ ] Generate property tests to check that all actions are reachable. This will
      help with checking the models, and detect errors early.

## Property based testing

- [ ] Incorporate support for state space exploration via
      [`SmallCheck`](https://hackage.haskell.org/package/smallcheck).

## Pretty printing

- [ ] Generate [`pfg`](https://github.com/pgf-tikz/pgf) diagrams from automata
      models.

## Documentation

- [ ] Write a better explanation of the data automata formalism.
- [ ] Scan the examples and extract information from the comments to explain the
      different parts of the framework (syntax, semantics, generation,
      conformance, etc).

## Stress test

  We need more examples to:
  - understand what this library can and can't do.
  - identify improvements to this library that'd make it useful in a wider
    variety of projects.

## To the infinity and beyond

- [ ] Translate models to model check tools for model cheking properties of the
      data automata models. Data automata are Turing complete, so this will
      require putting some constraints in the models that can be translated.
