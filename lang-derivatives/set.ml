(** Minimal set interface for comparing sets of tokens *)

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

  include Byte_set
  type elt = char

end

module Ord (A : Stdlib.Set.OrderedType) : S
  with type elt = A.t
= struct

  include Stdlib.Set.Make (A)

end
