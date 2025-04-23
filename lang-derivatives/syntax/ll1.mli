(** An LL(1) parser language implemented with derivatives.

    {2 Resources}

    - Romain Edelmann, Jad Hamza, Viktor Kunčak, “Zippy LL(1) parsing with
      derivatives”, PLDI 2020, https://doi.org/10.1145/3385412.3385992
    - {{: https://github.com/epfl-lara/scallion} epfl-lara/scallion} on Github
    - {{: https://github.com/epfl-lara/scallion-proofs>} epfl-lara/scallion-proofs} on Github
*)

module type S = sig

  include Core.S

  val parse : 'a t -> (token Seq.t -> 'a option) option
  (** Returns a parse function for the provided syntax, provided it is free from
      LL(1) conflicts. *)

end

module Make (T : Set.S) : S
  with type token = T.elt
  with type token_set = T.t

module Char : S
  with type token = char
  with type token_set = Byte_set.t
