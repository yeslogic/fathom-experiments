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

- “Linear-time parser combinators”
  by Neel Krishnaswami
  [[URL](https://semantic-domain.blogspot.com/2023/07/linear-time-parser-combinators.html)]
  [[Gist](https://gist.github.com/neel-krishnaswami/b1594c57433b7df2a143634a2fff3544)]
- “A typed, algebraic approach to parsing” (PLDI 2019)
  by Neel Krishnaswami and Jeremy Yallop
  [[DOI](https://doi.org/10.1145/3314221.3314625)]
  [[PDF](https://www.cl.cam.ac.uk/~nk480/parsing.pdf)]
  [[GitHub](https://github.com/yallop/ocaml-asp)]
- “flap: A Deterministic Parser with Fused Lexing” (PLDI 2023)
  by Jeremy Yallop, Ningning Xie and Neel Krishnaswami
  [[DOI](https://doi.org/10.1145/3591269)]
  [[PDF](https://dl.acm.org/doi/pdf/10.1145/3591269)]
  [[GitHub](https://github.com/yallop/ocaml-flap)]
