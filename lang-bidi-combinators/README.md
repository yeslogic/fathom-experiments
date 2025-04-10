# Bidirectional combinators

A library of bidirectional binary format combinators.

This is mainly based on “Composing Bidirectional Programs Monadically”,
which demonstrates an approach for defining data-dependent (i.e. monadic) codecs.

## Examples

Simple image format:

<!-- $MDX file=examples/image.ml -->
```ocaml
open Bidi_combinators

type t = {
  width : int;
  height : int;
  data : int32 array;
}

let width x = x.width
let height x = x.height
let data x = x.data

let format : t Format.value =
  let open Format.Syntax in

  let* width = width @= Format.int16_be
  and+ height = height @= Format.int16_be in
  let+ data = data @= Format.Array.repeat_len Format.int32_be (width * height) in

  { width; height; data }
```

More examples can be found in [the examples directory](./examples).

## Resources

- “Composing Bidirectional Programs Monadically”
  [[doi:10.1007/978-3-030-17184-1_6](https://doi.org/10.1007/978-3-030-17184-1_6)]
- [Towards monadic bidirectional serialization](https://blog.poisson.chat/posts/2016-10-12-bidirectional-serialization.html)
- codec: Easy bidirectional serialization in Haskell
  [[hackage:codec](https://hackage.haskell.org/package/codec)]
  [[github:chpatrick/codec](https://github.com/chpatrick/codec)]
- tomland: Bidirectional TOML serialization
  [[hackage:tomland](https://hackage.haskell.org/package/tomland)]
  [[github:kowainik/tomland](https://github.com/kowainik/tomland)]
- autodocodec: Self-documenting encoder and decoder
  [[hackage:autodocodec](https://hackage.haskell.org/package/autodocodec)]
  [[github:NorfairKing/autodocodec](https://github.com/NorfairKing/autodocodec)]
