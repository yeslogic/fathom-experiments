(** Signed integers *)

type int8 = private int [@@immediate]
type int16 = private int [@@immediate]
type nonrec int32 = int32 (* [@@immediate64] *)
type nonrec int64 = int64

module type S = sig

  include Unsigned.S

  val minus_one : t
  val abs : t -> t
  val shift_right_logical : t -> int -> t

end

module Int8 : sig

  type t = int8 [@@immediate]

  include S with type t := t

  val to_int : t -> int

end

module Int16 : sig

  type t = int16 [@@immediate]

  include S with type t := t

  val of_int8 : int8 -> t
  val to_int : t -> int

end

module Int32 : sig

  type t = int32 (* [@@immediate64] *)

  include module type of Stdlib.Int32 with type t := t
  include S with type t := t

  val of_int8 : int8 -> t
  val of_int16 : int16 -> t
  val to_int_opt : t -> int option

end

module Int64 : sig

  type t = int64

  include module type of Stdlib.Int64 with type t := t
  include S with type t := t

  val of_int8 : int8 -> t
  val of_int16 : int16 -> t
  val of_int32 : int32 -> t
  val to_int_opt : t -> int option

end
