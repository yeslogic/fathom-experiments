# flap: A Deterministic Parser with Fused Lexing

- **Authors**: Jeremy Yallop, Ningning Xie and Neel Krishnaswami
- **Conference**: PLDI 2023
- **DOI**: [doi:10.1145/3591269](https://doi.org/10.1145/3591269)
- **Source**: [gihub:yallop/ocaml-flap](https://github.com/yallop/ocaml-flap)
- **Abstract**:
  > Lexers and parsers are typically defined separately and connected by a token
  > stream. This separate definition is important for modularity and reduces the
  > potential for parsing ambiguity. However, materializing tokens as data
  > structures and case-switching on tokens comes with a cost.
  >
  > We show how to _fuse_ separately-defined lexers and parsers, drastically
  > improving performance without compromising modularity or increasing
  > ambiguity. We propose a deterministic variant of Greibach Normal Form that
  > ensures deterministic parsing with a single token of lookahead and makes
  > fusion strikingly simple, and prove that normalizing context free
  > expressions into the deterministic normal form is semantics-preserving.
  > Our staged parser combinator library, flap, provides a standard interface,
  > but generates specialized token-free code that runs two to six times faster
  > than ocamlyacc on a range of benchmarks.

<!-- markdownlint-disable no-inline-html -->
<details>
<summary>BibTeX</summary>

```bibtex
@article{10.1145/3591269,
author = {Yallop, Jeremy and Xie, Ningning and Krishnaswami, Neel},
title = {flap: A Deterministic Parser with Fused Lexing},
year = {2023},
issue_date = {June 2023},
publisher = {Association for Computing Machinery},
address = {New York, NY, USA},
volume = {7},
number = {PLDI},
url = {https://doi.org/10.1145/3591269},
doi = {10.1145/3591269},
abstract = {Lexers and parsers are typically defined separately and connected by
a token stream. This separate definition is important for modularity and reduces
the potential for parsing ambiguity. However, materializing tokens as data
structures and case-switching on tokens comes with a cost. We show how to fuse
separately-defined lexers and parsers, drastically improving performance without
compromising modularity or increasing ambiguity. We propose a deterministic
variant of Greibach Normal Form that ensures deterministic parsing with a single
token of lookahead and makes fusion strikingly simple, and prove that
normalizing context free expressions into the deterministic normal form is
semantics-preserving. Our staged parser combinator library, flap, provides a
standard interface, but generates specialized token-free code that runs two to
six times faster than ocamlyacc on a range of benchmarks.},
journal = {Proc. ACM Program. Lang.},
month = jun,
articleno = {155},
numpages = {24},
keywords = {fusion, lexing, multi-stage programming, optimization, parsing}
}
```

</details>
