open Bidi_combinators

module Header = struct

  type t = {
    width : int;
    height : int;
  }

  let width x = x.width
  let height x = x.height

  let format : (t, t) Format.t =
    let open Format.Syntax in

    let+ width = width @= Format.int16_be
    and+ height = height @= Format.int16_be in

    { width; height }

end

type t = {
  header : Header.t;
  data : int32 array;
}

let header x = x.header
let data x = x.data

let format =
  let open Format.Syntax in

  let* header = header @= Header.format in
  let+ data = data @= Format.Array.repeat_len Format.int32_be (header.width * header.height) in

  { header; data }
