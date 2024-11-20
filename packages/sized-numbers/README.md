# Fixed sized numbers for OCaml

Provides support for the following numeric types:

- Unsigned integers
  - `UInt8` (based on `int`)
  - `UInt16` (based on `int`)
  - `UInt32` (based on `int32`)
  - `UInt64` (based on `int64`)
- Signed integers
  - `Int8` (based on `int`)
  - `Int16` (based on `int`)
  - `Int32` (based on `int32`)
  - `Int64` (based on `int64`)
- Floating point numbers
  - `Float32` (based on `float`)
  - `Float64` (based on `float`)

The small integer types are emulated in terms of OCaml’s `int` type,
so will not perform as well as if using them directly in a systems programming language.
They are however useful for implementing compilers and interpreters for programming languages
that need to provide support for these numeric types.

## Comparison to other packages

- [github:janestreet/int_repr](https://github.com/janestreet/int_repr/):
  depends on `base`
- [github:yallop/ocaml-integers](https://github.com/yallop/ocaml-integers/):
  lacks support for small integers, depends on native code, and requires
  additional support to work with javascript backends
