module Void = struct

  type t = |

  let absurd : t -> 'a =
    function  _ -> .

  let equal : t -> t -> bool =
    function _ -> .

  let compare : t -> t -> int =
    function _ -> .

  let to_string : t -> string =
    function _ -> .

end

module IndexedMonad = struct

  module type S = sig

    type ('a, 'i) m

    val pure : 'a -> ('a, 'i) m
    val bind : ('a, 'i) m -> ('a -> ('b, 'i) m) -> ('b, 'i) m

  end

end
