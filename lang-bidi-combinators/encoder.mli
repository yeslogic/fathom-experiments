(** The type of an encoder, parameterized by:

    - ['a]: the type of the value after encoding (used for dependent encoders)
    - ['c]: the type of the value being encoded
*)
type ('c, 'a) t = Buffer.t -> 'c -> 'a option

val pure : 'a -> ('c, 'a) t
val map : ('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t
val both : ('c, 'a) t -> ('c, 'b) t -> ('c, 'a * 'b) t
val apply : ('c, 'a -> 'b) t -> ('c, 'a) t -> ('c, 'b) t
val bind : ('c, 'a) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
val fail : ('c, 'a) t
val alt : ('c, 'a) t -> ('c, 'a) t -> ('c, 'a) t

val dimap : ('a -> 'b) -> ('d -> 'c) -> ('c, 'a) t -> ('d, 'b) t
val comap : ('d -> 'c) -> ('c, 'a) t -> ('d, 'a) t

module Syntax : sig

  val ( <$> ) : ('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t
  val ( <*> ) : ('a, 'b -> 'c) t -> ('a, 'b) t -> ('a, 'c) t
  val ( </> ) : ('c, 'a) t -> ('c, 'a) t -> ('c, 'a) t

  val ( @= ) : ('d -> 'c) -> ('c, 'a) t -> ('d, 'a) t

  val ( let+ ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
  val ( and+ ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  val ( let* ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
  val ( and* ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t

end
