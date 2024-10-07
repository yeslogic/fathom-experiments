# Linear-time parser combinators

- **Author**: Neel Krishnaswami
- **Date**: Friday, July 21, 2023
- **Url**: <https://semantic-domain.blogspot.com/2023/07/linear-time-parser-combinators.html>
- **Source**: [gist:b1594c57433b7df2a143634a2fff3544](https://gist.github.com/neel-krishnaswami/b1594c57433b7df2a143634a2fff3544)

## Notes

[@brendanzab](https://github.com/brendanzab):

This blog post seems to be based on their prior papers[^krishnaswami-2019][^yallop-2023],
but drops the lexer.
I had a tried replicating it in the [lang-ll1-combinators](../../lang-ll1-combinators/) project.

The way they use bit sets to detect ambiguities reminds me a great deal of the approach we are using in [doodle](./doodle.md),
but limited to one character of lookahead.

[^krishnaswami-2019]: [A typed, algebraic approach to parsing](./a-typed-algebraic-approach-to-parsing.md)
[^yallop-2023]: [flap: A Deterministic Parser with Fused Lexing](./flap-a-deterministic-parser-with-fused-lexing.md)
