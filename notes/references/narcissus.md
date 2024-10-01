# Narcissus: correct-by-construction derivation of decoders and encoders from binary formats

| Field       | Value |
| ----------- | ----- |
| Authors     | Benjamin Delaware, Sorawit Suriyakarn, Clément Pit-Claudel, Qianchuan Ye, Adam Chlipala |
| Conference  | ICFP |
| Date        | 2019-07-26 |
| DOI         | [10.1145/3341686](https://doi.org/10.1145/3341686) |

## Abstract

It is a neat result from functional programming that libraries of _parser
combinators_ can support rapid construction of decoders for quite a range of
formats. With a little more work, the same combinator program can denote both a
decoder and an encoder. Unfortunately, the real world is full of gnarly formats,
as with the packet formats that make up the standard Internet protocol stack.
Most past parser-combinator approaches cannot handle these formats, and the few
exceptions require redundancy – one part of the natural grammar needs to be
hand-translated into hints in multiple parts of a parser program. We show how to
recover very natural and nonredundant format specifications, covering all popular
network packet formats and generating both decoders and encoders automatically.
The catch is that we use the Coq proof assistant to derive both kinds of
artifacts using tactics, automatically, in a way that guarantees that they form
inverses of each other. We used our approach to reimplement packet processing
for a full Internet protocol stack, inserting our replacement into the
OCaml-based MirageOS unikernel, resulting in minimal performance degradation.

## Notes

[@brendanzab](https://github.com/brendanzab):

The source code can be found here: <https://github.com/mit-plv/fiat/tree/master/src/Narcissus>.

This approach to binary data formats uses a DSL embedded in Coq.
It’s impressive to see how this can be used to generate formally verified parsers,
but it does seem like there would be a steep learning curve to using it (you need to learn Coq as well).

The data descriptions are also challenging to read.
Perhaps using a similar approach in Lean 4 might allow for cleaner data descriptions,
given it’s more powerful support for custom syntax?

I found it interesting to see how they defined the binary formats in terms of encoder/decoder pairs,
as opposed to as a closed universe of data descriptions like in [The Power of Pi](./the-power-of-pi.md).
This means that the formats are open to extension,
which reminds me of the co-inductive universes proposed by Shulman[^1].

[^1]: [Towards third generation HOTT - Part 3: Univalent universes](https://home.sandiego.edu/~shulman/papers/hott-cmu-day3.pdf)