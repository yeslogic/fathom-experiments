# Zippy LL(1) parser language

An LL(1) parser language based on on “Zippy LL(1) parsing with derivatives”[^zippy].

The hope is to experiment with parsing-with-derivatives, and explore compiling
NFA-style syntax descriptions to DFA-style syntax descriptions, as is
implemented in Daedalus[^daedalus-paper][^daedalus-code].

- <https://github.com/epfl-lara/scallion>
- <https://github.com/epfl-lara/scallion-proofs>

[^zippy]: Romain Edelmann, Jad Hamza, Viktor Kunčak, “Zippy LL(1) parsing with derivatives”, 2020 [[DOI](https://doi.org/10.1145/3385412.3385992)]
[^daedalus-paper]: Iavor S. Diatchki et. al. “Daedalus: Safer Document Parsing”, 2024 [[DOI](https://doi.org/10.1145/3656410)]
[^daedalus-code]: <https://github.com/GaloisInc/daedalus/blob/master/daedalus-core/src/Daedalus/Core/Determinize.hs>
