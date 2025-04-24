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
