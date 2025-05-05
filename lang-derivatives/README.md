# Derivative-based parser experiments

The hope for this project is to experiment with parsing-with-derivatives, e
xploring compiling NFA-style syntax descriptions to DFA-style syntax
descriptions, as is implemented in Daedalus[^daedalus-paper][^daedalus-code].

The main modules of interest are:

| Name | Description |
| ---- | ----------- |
| [`Ll_naive`] | Top-down, left-to-right parsing with full backtracking. |
| [`Ll1_simple`] | Top-down, LL(1) parsing with derivatives, recomputing LL(1) properties as needed. Compiles to the [`Ll1_det`] language. |
| [`Ll1_record`] | Top-down, LL(1) parsing with derivatives, precomputing LL(1) properties ahead of time. Compiles to the [`Ll1_det`] language. |
| [`Ll1_det`] | Non-backtracking parsing with a single token of lookahead. |

[`Ll_naive`]: lib/syntax/ll_naive.ml
[`Ll1_simple`]: lib/syntax/ll1_simple.ml
[`Ll1_record`]: lib/syntax/ll1_record.ml
[`Ll1_det`]: lib/syntax/ll1_det.ml

## Resources

- Romain Edelmann, Jad Hamza, Viktor Kunčak, “Zippy LL(1) parsing with derivatives”, PLDI 2020, [[DOI](https://doi.org/10.1145/3385412.3385992)]
- [epfl-lara/scallion](https://github.com/epfl-lara/scallion) on Github
- [epfl-lara/scallion-proofs](https://github.com/epfl-lara/scallion-proofs) on Github

[^daedalus-paper]: Iavor S. Diatchki et. al. “Daedalus: Safer Document Parsing”, PLDI 2024 [[DOI](https://doi.org/10.1145/3656410)]
[^daedalus-code]: [Daedalus/Core/Determinize.hs](https://github.com/GaloisInc/daedalus/blob/347ab98b175201e5e41ee5a935fa2a6ed5d89cb2/daedalus-core/src/Daedalus/Core/Determinize.hs)
