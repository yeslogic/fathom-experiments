module type S = sig

  type elt

  type t

  val empty : t
  val singleton : elt -> t
  val union : t -> t -> t
  val mem : elt -> t -> bool
  val disjoint : t -> t -> bool

end

module Char : S
  with type elt = char
  with type t = Byte_set.t
= struct

  type elt = char

  include Byte_set

end

module Make_ordered (Ord : Set.OrderedType) : S
  with type elt = Ord.t
= struct

  include Set.Make (Ord)

end
