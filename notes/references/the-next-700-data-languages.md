# The Next 700 Data Description Languages

| Field       | Value |
| ----------- | ----- |
| Authors     | Kathleen Fisher, Yitzhak Mandelbaum, David Walker |
| Conference  | POPL’06 |
| Date        | January 11–13, 2006 |
| DOI         | [10.1145/1111037.1111039](https://doi.org/10.1145/1111037.1111039) |
| PDF Link    | <https://www.cs.princeton.edu/~dpw/papers/700popl06.pdf> |

## Abstract

In the spirit of Landin, we present a calculus of dependent types to
serve as the semantic foundation for a family of languages called
_data description languages_. Such languages, which include PADS,
DATASCRIPT, and PACKETTYPES, are designed to facilitate pro-
gramming with _ad hoc data_, i.e., data not in well-behaved relational
or XML formats. In the calculus, each type describes the physical
layout and semantic properties of a data source. In the semantics,
we interpret types simultaneously as the in-memory representation
of the data described and as parsers for the data source. The parsing
functions are robust, automatically detecting and recording errors
in the data stream without halting parsing. We show the parsers are
type-correct, returning data whose type matches the simple-type
interpretation of the specification. We also prove the parsers are
“error-correct,” accurately reporting the number of physical and se-
mantic errors that occur in the returned data. We use the calculus to
describe the features of various data description languages, and we
discuss how we have used the calculus to improve PADS.

## Notes

[@brendanzab](https://github.com/brendanzab):

This was inspired the work on Fathom.
The language (PADS), is intended for use on text-based formats like log files,
but we were hoping to apply it to binary formats like OpenType.

I think their use of the term “type” for _data descriptions_ in the IPADS syntax and DDC syntax is rather misleading, so beware!
It led me astray for quite while when trying to implement a dependently typed data description language.
I think a better approach is to treat these as an inductive-recursive _universe_ of data descriptions,
which you can see in [The Power of Pi](./the-power-of-pi.md).
This is the approach I used in later versions of Fathom.
