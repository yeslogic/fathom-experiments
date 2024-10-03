# Daedalus: Safer Document Parsing

- **Authors**: Iavor S. Diatchki, Mike Dodds, Harrison Goldstein, Bill Harris, David A. Holland, Benoit Razet, Cole Schlesinger, Simon Winwood
- **Conference**: PLDI
- **Published**: 20 June 2024
- **DOI**: [10.1145/3656410](https://doi.org/10.1145/3656410)
- **Abstract**:
  > Despite decades of contributions to the theoretical foundations of parsing
  > and the many tools available to aid in parser development, many security
  > attacks in the wild still exploit parsers. The issues are myriad—flaws in
  > memory management in contexts lacking memory safety, flaws in syntactic or
  > semantic validation of input, and misinterpretation of hundred-page-plus
  > standards documents. It remains challenging to build and maintain parsers
  > for common, mature data formats. In response to these challenges, we present
  > Daedalus, a new domain-specific language (DSL) and toolchain for writing
  > safe parsers. Daedalus is built around functional-style parser combinators,
  > which suit the rich data dependencies often found in complex data formats.
  > It adds domain-specific constructs for stream manipulation, allowing the
  > natural expression of parsing noncontiguous formats. Balancing between
  > expressivity and domain-specific constructs lends Daedalus specifications
  > simplicity and leaves them amenable to analysis. As a stand-alone DSL,
  > Daedalus is able to generate safe parsers in multiple languages, currently
  > C++ and Haskell. We have implemented 20 data formats with Daedalus,
  > including two large, complex formats—PDF and NITF—and our evaluation shows
  > that Daedalus parsers are concise and performant. Our experience with PDF
  > forms our largest case study. We worked with the PDF Association to build a
  > reference implementation, which was subject to a red-teaming exercise along
  > with a number of other PDF parsers and was the only parser to be found free
  > of defects.

<!-- markdownlint-disable no-inline-html -->
<details>
<summary>BibTeX</summary>

```bibtex
@article{10.1145/3656410,
author = {Diatchki, Iavor S. and Dodds, Mike and Goldstein, Harrison and Harris,
Bill and Holland, David A. and Razet, Benoit and Schlesinger, Cole and Winwood, Simon},
title = {Daedalus: Safer Document Parsing},
year = {2024},
issue_date = {June 2024},
publisher = {Association for Computing Machinery},
address = {New York, NY, USA},
volume = {8},
number = {PLDI},
url = {https://doi.org/10.1145/3656410},
doi = {10.1145/3656410},
abstract = {Despite decades of contributions to the theoretical foundations of
parsing and the many tools available to aid in parser development, many security
attacks in the wild still exploit parsers. The issues are myriad—flaws in memory
management in contexts lacking memory safety, flaws in syntactic or semantic
validation of input, and misinterpretation of hundred-page-plus standards
documents. It remains challenging to build and maintain parsers for common,
mature data formats. In response to these challenges, we present Daedalus, a new
domain-specific language (DSL) and toolchain for writing safe parsers. Daedalus
is built around functional-style parser combinators, which suit the rich data
dependencies often found in complex data formats. It adds domain-specific
constructs for stream manipulation, allowing the natural expression of parsing
noncontiguous formats. Balancing between expressivity and domain-specific
constructs lends Daedalus specifications simplicity and leaves them amenable to
analysis. As a stand-alone DSL, Daedalus is able to generate safe parsers in
multiple languages, currently C++ and Haskell. We have implemented 20 data
formats with Daedalus, including two large, complex formats—PDF and NITF—and our
evaluation shows that Daedalus parsers are concise and performant. Our
experience with PDF forms our largest case study. We worked with the PDF
Association to build a reference implementation, which was subject to a
red-teaming exercise along with a number of other PDF parsers and was the only
parser to be found free of defects.},
journal = {Proc. ACM Program. Lang.},
month = jun,
articleno = {180},
numpages = {25},
keywords = {Format definition languages, NITF, PDF, binary data formats}
}
```

</details>
<!-- markdownlint-restore -->
