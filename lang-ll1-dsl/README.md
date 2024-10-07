# Linear-time parser DSL

This is a linear time parser DSL, inspired by Neel Krishnaswami’s blog post.
Instead of being an embedded domain specific language (see the
[ll1-combinators](../lang-ll1-combinators) experiment), it is designed as an
external DSL.

In the future we could alter the language to support more lookahead using prefix
trees, as explored in [yeslogic/doodle](https://github.com/yeslogic/doodle).

## Todo List

Formats:

- [x] Empty formats
- [x] Byte formats
- [x] Byte range formats
- [x] Concatenation formats
- [x] Alternation formats
- [ ] String formats
- [ ] Repeat formats
- [ ] Fixpoint formats
- [x] Map formats
- [x] Bind formats
- [x] Record formats

Top-level definitions:

- [x] Simple format definitions
- [ ] Parameterised format definitions
  - [ ] Type parameters
  - [ ] Format parameters
  - [ ] Compile-time term parameters
  - [ ] Run-time term parameters
- [ ] Mutually recursive format definitions

Static analysis:

- [x] LL(1) ambiguity analysis
- [ ] LL(k) ambiguity analysis (prefix trees?)
- [x] Representation type checking

Parsing:

- [x] Tree-walking parser
- [ ] Rust parser compiler

## References

- [Linear-time parser combinators](../notes/references/linear-time-parser-combinators.md)
- [A typed, algebraic approach to parsing](../notes/references/a-typed-algebraic-approach-to-parsing.md)
- [flap: A Deterministic Parser with Fused Lexing](../notes/references/flap-a-deterministic-parser-with-fused-lexing.md)
