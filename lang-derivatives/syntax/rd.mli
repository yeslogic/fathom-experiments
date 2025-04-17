(** Top-down parser semantics, implemented with recursive-descent and
    backtracking.
*)

module type S = sig

  include Core.S

  val parse : 'a t -> token Seq.t -> ('a * token Seq.t) option

end

module Make (T : Token.S) : S
  with type token = T.t
  with type token_set = T.Set.t

module Char : S
  with type token = char
  with type token_set = Byte_set.t
