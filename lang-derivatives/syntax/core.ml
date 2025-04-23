(** Core constructors for defining syntax descriptions *)

module type S = sig

  type token
  type token_set

  type _ t

  val elem : token_set -> token t
  val fail : 'a t
  val pure : 'a -> 'a t
  val alt : 'a t -> 'a t -> 'a t
  val seq : 'a t -> 'b t -> ('a * 'b) t
  val map : ('a -> 'b) -> 'a t -> 'b t

end

module Make (T : Set.S) = struct

  type token = T.elt
  type token_set = T.t

  type _ t =
    | Elem : token_set -> token t
    | Fail : 'a t
    | Pure : 'a -> 'a t
    | Alt : 'a t * 'a t -> 'a t
    | Seq : 'a t * 'b t -> ('a * 'b) t
    | Map : ('a -> 'b) * 'a t -> 'b t
    (* TODO: variables *)

  let elem t = Elem t
  let fail = Fail
  let pure x = Pure x
  let alt s1 s2 = Alt (s1, s2)
  let seq s1 s2 = Seq (s1, s2)
  let map f s = Map (f, s)

end
