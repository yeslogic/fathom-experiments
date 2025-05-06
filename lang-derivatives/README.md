# Derivative-based parser experiments

This project is centered around parsing with derivatives, currently with a focus
on exploring the background behind Daedalus’ determinization pass[^daedalus-paper][^daedalus-code].

## Project overview

The main modules of interest are:

| Name | Description |
| ---- | ----------- |
| [`Peg`] | Top-down parsers with full backtracking and left-biased alternation. |
| [`Ll1_derive`] | Top-down parsing with derivatives, recomputing LL(1) properties as needed. Also compiles to the [`Ll1`] language. |
| [`Ll1_derive_pre`] | Top-down parsing with derivatives, precomputing LL(1) properties ahead of time. Also compiles to the [`Ll1`] language. |
| [`Ll1`] | Top-down parsers with a single token of lookahead. |

[`Peg`]: lib/syntax/ll.ml
[`Ll1_derive`]: lib/syntax/ll1_derive.ml
[`Ll1_derive_pre`]: lib/syntax/ll1_derive_precomp.ml
[`Ll1`]: lib/syntax/ll1.ml

## Resources

- Romain Edelmann, Jad Hamza, Viktor Kunčak, “Zippy LL(1) parsing with derivatives”, PLDI 2020, [[DOI](https://doi.org/10.1145/3385412.3385992)]
- [epfl-lara/scallion](https://github.com/epfl-lara/scallion) on Github
- [epfl-lara/scallion-proofs](https://github.com/epfl-lara/scallion-proofs) on Github

[^daedalus-paper]: Iavor S. Diatchki et. al. “Daedalus: Safer Document Parsing”, PLDI 2024 [[DOI](https://doi.org/10.1145/3656410)]
[^daedalus-code]: [Daedalus/Core/Determinize.hs](https://github.com/GaloisInc/daedalus/blob/347ab98b175201e5e41ee5a935fa2a6ed5d89cb2/daedalus-core/src/Daedalus/Core/Determinize.hs)
