# A simple binary format DSL

A simple, typed binary format DSL that elaborates to a core language and
compiles to a recursive descent parser in Rust. This could be used as the basis
of future language experiments.

The surface language is unified into a single term syntax that is disambiguated
into types, expressions and formats during elaboration. This could be simplified
later, but leaves the door open for richer forms of type and format
parameterisation in the future.

Record types and formats must be defined at the top-level, which makes
compilation to languages with nominal types easier, but this could be changed
later if desired.

## Future work

Additional experiments could be added as additional projects, exploring the following:

- Steppable interpreter (similar to the CEK machine)
- Documentation generation
- Format-directed rendering of parsed data
- Other parsing approaches
  - Early parser generator
  - Table-based parser generator
  - Linear-time parser combinators
  - Prefix-tree based ambiguity analysis (similar to doodle)
- Fuzzer generation

Additional language features:

- [x] Metavariables
- [ ] Untagged alternative formats
- [ ] Tagged alternative formats
- [ ] More integer types
- [ ] Postponed elaboration
- [ ] Refinement types
- [ ] Parameterised types
- [ ] Parameterised formats
- [ ] Parameterised expressions
- [ ] Module imports
- [ ] Bidirectional formats
  - Custom formats from encoder/decoder pairs
- [ ] FFI for external encoder/decoders

## Resources

- [language-garden/elab-stlc-bidirectional](https://github.com/brendanzab/language-garden/tree/main/elab-stlc-bidirectional)
- [language-garden/elab-stlc-bidirectional-stratify](https://github.com/brendanzab/language-garden/tree/main/elab-stlc-bidirectional-stratify)
