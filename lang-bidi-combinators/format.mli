(** The type of a format, parameterized by:

    - ['a]: the type of the decoded value and the type of the value after
            encoding (used for dependent encoders)
    - ['c]: the type of the value being encoded
*)
type ('c, 'a) t = {
  decode : 'a Decoder.t;
  encode : ('c, 'a) Encoder.t;
}

type 'a value = ('a, 'a) t

val pure : 'a -> ('c, 'a) t
val map : ('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t
val both : ('c, 'a) t -> ('c, 'b) t -> ('c, 'a * 'b) t
val apply : ('c, 'a -> 'b) t -> ('c, 'a) t -> ('c, 'b) t
val bind : ('c, 'a) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
val fail : ('c, 'a) t
val alt : ('c, 'a) t -> ('c, 'a) t -> ('c, 'a) t

val dimap : ('a -> 'b) -> ('d -> 'c) -> ('c, 'a) t -> ('d, 'b) t
val comap : ('d -> 'c) -> ('c, 'a) t -> ('d, 'a) t

val const : 'a value -> 'a -> ('c, unit) t
val unused : 'a value -> default:'a -> ('c, unit) t

module Syntax : sig

  val ( <$> ) : ('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t
  val ( <*> ) : ('a, 'b -> 'c) t -> ('a, 'b) t -> ('a, 'c) t
  val ( </> ) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

  val ( @= ) : ('d -> 'c) -> ('c, 'a) t -> ('d, 'a) t

  val ( let+ ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
  val ( and+ ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  val ( let* ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
  val ( and* ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t

end

open Sized_numbers

val uint32_be : uint32 value
val uint32_le : uint32 value
val uint64_be : uint64 value
val uint64_le : uint64 value

val int8 : int value
val int16_be : int value
val int16_le : int value
val int32_be : int32 value
val int32_le : int32 value
val int64_be : int64 value
val int64_le : int64 value

module List : sig

  val repeat_len : 'a value -> int -> 'a list value

end

module Array : sig

  val repeat_len : 'a value -> int -> 'a array value

end
