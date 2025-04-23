(** Backtracking, recursive descent parser semantics *)

module type S = sig

  include Core.S

  val parse : 'a t -> token Seq.t -> ('a * token Seq.t) option

end

module Make (T : Set.S) : S
  with type token = T.elt
  with type token_set = T.t

module Char : S
  with type token = char
  with type token_set = Byte_set.t
