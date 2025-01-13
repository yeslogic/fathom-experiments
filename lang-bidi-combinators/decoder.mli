(** The type of a decoder *)
type 'a t = bytes -> int -> ('a * int) option

val pure : 'a -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val both : 'a t -> 'b t -> ('a * 'b) t
val apply : ('a -> 'b) t -> 'a t -> 'b t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val fail : 'a t
val alt : 'a t -> 'a t -> 'a t

module Syntax : sig

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( </> ) : 'a t -> 'a t -> 'a t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t

end
