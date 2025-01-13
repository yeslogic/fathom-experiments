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
