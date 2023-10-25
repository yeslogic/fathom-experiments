module Void : sig
  (** The void type. *)

  type t = |
  (** The void type that has no constructors. *)

  val absurd : t -> 'a
  (** [absurd e] never returns. *)

  val equal : t -> t -> bool
  (** [equal e1 e2] never returns. *)

  val compare : t -> t -> int
  (** [compare e1 e2] never returns. *)

  val to_string : t -> string
  (** [to_string e] never returns. *)

end

module IndexedMonad : sig
  (** Indexed monads. *)

  module type S = sig

    type ('a, 'i) m

    val pure : 'a -> ('a, 'i) m
    val bind : ('a, 'i) m -> ('a -> ('b, 'i) m) -> ('b, 'i) m

  end

end
