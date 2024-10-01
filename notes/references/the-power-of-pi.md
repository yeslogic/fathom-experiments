# The Power of Pi

| Field       | Value |
| ----------- | ----- |
| Authors     | Nicolas Oury, Wouter Swierstra |
| Conference  | ICFP '08 |
| Date        | September 20, 2008 |
| DOI         | [10.1145/1411204.1411213](https://doi.org/10.1145/1411204.1411213) |
| PDF Link    | <https://webspace.science.uu.nl/~swier004/publications/2008-icfp.pdf> |

## Abstract

This paper exhibits the power of programming with dependent
types by dint of embedding three domain-specific languages: Cryptol,
a language for cryptographic protocols; a small data description
language; and relational algebra. Each example demonstrates particular
design patterns inherent to dependently-typed programming.
Documenting these techniques paves the way for further research
in domain-specific embedded type systems.

## Notes

[@brendanzab](https://github.com/brendanzab):

This paper demonstrates a series of practical use cases for dependently typed programming,
including an example of an embedded data description language in Section 3.
The relevant code is listed below:

```agda
data U : Set where
  BIT : U
  CHAR : U
  NAT : U
  VEC : U → Nat → U

el : U → Set
el BIT        = Bit
el CHAR       = Char
el NAT        = Nat
el (VEC u n)  = Vec (el u) n

data Format : Set where
  Bad   : Format
  End   : Format
  Base  : U → Format
  Plus  : Format → Format → Format
  Skip  : Format → Format → Format
  Read  : (f : Format) → (⟦f⟧ → Format) → Format

⟦_⟧ : Format → Set
⟦Bad⟧         = Empty
⟦End⟧         = Unit
⟦Base u⟧      = el u
⟦Plus f₁ f₂⟧  = Either ⟦f₁⟧ ⟦f₂⟧
⟦Read f₁ f₂⟧  = Sigma ⟦f₁⟧ (λx → ⟦f₂ x⟧)
⟦Skip _ f⟧    = ⟦f⟧

data Sigma (A : Set) (B : A → Set) : Set where
  Pair : (x : A) → B x → Sigma A B

data Either (A : Set) (B : Set) : Set where
  Inl : A → Either A B
  Inr : B → Either A B
```

The DSL uses and inductive-recursive universe of file formats called `Format`,
which is analogous to `τ` in DDC[^ddc].
The `⟦_⟧ : Format → Set` function on the other hand is analogous to the `⟦τ⟧rep = σ` relation in DDC.
Unlike DDC dependent types are used to track dynamic information in the types, like the lengths of arrays.

The “universe pattern” described in Section 3 seems pretty close to how you’d embed Tarski universes[^tarski-nlab] in a type theory.

Note that the `Read` formats require that the parsed values are present in the resulting decoded data,
as part of a sigma type.
This could be a performance issue if that information is only in the type,
but this could be alleviated with quantitative type theory[^nuttin][^qtt] like in Idris[^idris-qtt],
or an run-time irrelevance like in recent versions of Agda[^agda-irrel].

I attempted to use a similar approach to this in later implementations of Fathom.
One issue I ran into was interleaving computed data inside formats,
when you might not want those to appear in the resulting data structure.
I now feel like this might be a similar issue to the the `Read` formats above.

[^ddc]: [The Next 700 Data Description Languages](./the-next-700-data-languages.md)
[^nuttin]: [I Got Plenty o’ Nuttin’](https://personal.cis.strath.ac.uk/conor.mcbride/PlentyO-CR.pdf)
[^qtt]: [Syntax and Semantics of Quantitative Type Theory](https://bentnib.org/quantitative-type-theory.pdf)
[^idris-qtt]: [A Crash Course in Idris 2: Multiplicities](https://idris2.readthedocs.io/en/latest/tutorial/multiplicities.html)
[^tarski-nlab]: [Tarski universe](https://ncatlab.org/nlab/show/Tarski+universe) on the nLab
[^agda-irrel]: [Agda Language Reference: Run-time Irrelevance](https://agda.readthedocs.io/en/v2.6.1.1/language/runtime-irrelevance.html)
