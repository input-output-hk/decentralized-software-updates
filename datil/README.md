## TODO's

### Expressiveness

- [ ] Add support for richer expressions. Some examples of functions what can be
      added to the language include:
  - [ ] Union override for relations
  - [ ] Membership testing for relation

### Sanity checking models

- [ ] Check that all variables in the `RunnableModel.automata` are declared in
      `RunnableModel.initialMemory`. We could also check types as well.
- [ ] Generate property tests to check that all actions are reachable. This will
      help with checking the models, and detect errors early.

### Documentation

- [ ] Scan the examples and extract information from the comments to explain the
      different parts of the framework (syntax, semantics, generation,
      conformance, etc).
