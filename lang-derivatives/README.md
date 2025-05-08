# Derivative-based parser experiments

This project is centered around parsing with derivatives, currently with a focus
on exploring the background behind Daedalus’ determinization pass.

## Project overview

The main modules of interest are:

| Name | Description |
| ---- | ----------- |
| [`Peg`] | Top-down parsers with full backtracking and left-biased alternation. |
| [`Ll1_properties`] | LL(1) properties of syntaxes, used in [`Ll1_derive`]. |
| [`Ll1_derive`] | Top-down parsing with derivatives. Compiles to the [`Ll1_case_tree`] language. |
| [`Ll1_case_tree`] | Top-down parsers with a single token of lookahead, expressed as case trees. |

[`Peg`]: lib/syntax/ll.ml
[`Ll1_derive`]: lib/syntax/ll1_derive.ml
[`Ll1_properties`]: lib/syntax/ll1_properties.ml
[`Ll1_case_tree`]: lib/syntax/ll1_case_tree.ml

## Resources

- Iavor S. Diatchki et. al. “Daedalus: Safer Document Parsing”, PLDI 2024.
  [[DOI](https://doi.org/10.1145/3656410)]
  [[PDF](https://dl.acm.org/doi/pdf/10.1145/3656410)]
  - [Daedalus/Core/Determinize.hs](https://github.com/GaloisInc/daedalus/blob/347ab98b175201e5e41ee5a935fa2a6ed5d89cb2/daedalus-core/src/Daedalus/Core/Determinize.hs)
- Romain Edelmann, Jad Hamza, and Viktor Kunčak, “Zippy LL(1) parsing with derivatives”, PLDI 2020.
  [[DOI](https://doi.org/10.1145/3385412.3385992)]
  [[PDF](https://lara.epfl.ch/~kuncak/papers/EdelmannETAL20ZippyLLParsingDerivatives.pdf)]
  - [epfl-lara/scallion](https://github.com/epfl-lara/scallion)
  - [epfl-lara/scallion-proofs](https://github.com/epfl-lara/scallion-proofs)
- S. Doaitse Swierstra, and Luc Duponcheel, “Deterministic, error-correcting combinator parsers”, Advanced Functional Programming, 1996.
  [[DOI](https://doi.org/10.1007/3-540-61628-4_7)]
  [[PDF](https://www.cs.tufts.edu/~nr/cs257/archive/doaitse-swierstra/LL1.pdf)]
