# A Tutorial Implementation of a Dependently Typed Lambda Calculus

- **Authors**: Andres Löh, Conor McBride, Wouter Swierstra
- **Date**: 2009
- **Webpage**: <https://www.andres-loeh.de/LambdaPi/>
- **Abstract**:
  > We present the type rules for a dependently-typed core calculus together with a
  > straightforward implementation in Haskell. We explicitly highlight the changes
  > necessary to shift from a simply-typed lambda calculus to the dependently-typed
  > lambda calculus. We also describe how to extend our core language with data
  > types and write several small example programs. The paper is accompanied by an
  > executable interpreter and example code that allows immediate experimentation
  > with the system we describe.

## Notes

[@brendanzab](https://github.com/brendanzab):

I originally came across this paper as part of trying to get my head around dependent type checking.
At the time it wasn’t clear to me how computation and type checking interacted.
This paper did a good job of getting me up to speed with this,
though I did end up implementing it inefficiently.

They start with a simply typed lambda calculus in a similar style to TaPL[^1],
first showing the typing and evaluation rules in the style of natural deduction,
and then an implementation of those rules in Haskell.
Later on they add support for dependent types as a diff on the existing rules and implementation,
which I found very enlightening.

Their approach to implementing evaluation is actually a form of _normalisation-by-evauation_,
but I don’t recall them saying this in the paper.
I remember being confused by it, and as a result implementing it inefficiently,
attempting to use my variable binding library[^2] (based on Unbound).

It took me a while to discover _semantic typechecking_ based on a recommendation from Jon Sterling,
which helped me better understand the approach described in this paper.

Later on András Kovacs created the [elaboration-zoo](./elaboration-zoo.md),
which was a great help as well,
especially in showing how to extend these kinds of type checkers to support metavariables and unification,
which is a bit of a dark art.

In the future I’d love to see a more up-to-date version of this paper that
uses implementations closer to those found in the elaboration-zoo.

[^1]: [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/)
[^2]: [github:brendanzab/moniker](https://github.com/brendanzab/moniker)
