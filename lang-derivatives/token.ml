module type S = sig

  type t

  module Set : sig

    type elt = t

    type t

    val empty : t
    val singleton : elt -> t
    val union : t -> t -> t
    val mem : elt -> t -> bool
    val disjoint : t -> t -> bool

  end

end

module Char : S
  with type t = char
  with type Set.t = Byte_set.t
= struct

  type t = char

  module Set = struct

    type elt = t

    include Byte_set

  end

end

module Make_ordered (Ord : Set.OrderedType) : S
  with type t = Ord.t
= struct

  type t = Ord.t

  module Set = Set.Make (Ord)

end
