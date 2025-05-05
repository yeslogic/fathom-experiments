# Derivative-based parser experiments

The hope is to experiment with parsing-with-derivatives, and explore compiling
NFA-style syntax descriptions to DFA-style syntax descriptions, as is
implemented in Daedalus[^daedalus-paper][^daedalus-code].

## Resources

- Romain Edelmann, Jad Hamza, Viktor Kunčak, “Zippy LL(1) parsing with derivatives”, PLDI 2020, [[DOI](https://doi.org/10.1145/3385412.3385992)]
- [epfl-lara/scallion](https://github.com/epfl-lara/scallion) on Github
- [epfl-lara/scallion-proofs](https://github.com/epfl-lara/scallion-proofs) on Github

[^daedalus-paper]: Iavor S. Diatchki et. al. “Daedalus: Safer Document Parsing”, PLDI 2024 [[DOI](https://doi.org/10.1145/3656410)]
[^daedalus-code]: [Daedalus/Core/Determinize.hs](https://github.com/GaloisInc/daedalus/blob/347ab98b175201e5e41ee5a935fa2a6ed5d89cb2/daedalus-core/src/Daedalus/Core/Determinize.hs)
