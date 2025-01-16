(** Unsigned integers *)

type uint8 = private int [@@immediate]
type uint16 = private int [@@immediate]
type uint32 = private int32 (* [@@immediate64] *)
type uint64 = private int64

module type S = sig

  type t

  val num_bits : int
  val num_bytes : int
  val zero : t
  val one : t
  val min_int : t
  val max_int : t

  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val succ : t -> t
  val pred : t -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val min : t -> t -> t
  val max : t -> t -> t

  val of_string : string -> t
  val of_string_opt : string -> t option
  val to_string : t -> string

  val pp : Format.formatter -> t -> unit [@@toplevel_printer]

  val seeded_hash : int -> t -> int
  val hash : t -> int

  module O : sig

    val ( ~- ) : t -> t
    val ( ~+ ) : t -> t

    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t

    val ( = ) : t -> t -> bool
    val ( <> ) : t -> t -> bool
    val ( < ) : t -> t -> bool
    val ( > ) : t -> t -> bool
    val ( <= ) : t -> t -> bool
    val ( >= ) : t -> t -> bool

    val ( land ) : t -> t -> t
    val ( lor ) : t -> t -> t
    val ( lxor ) : t -> t -> t
    val ( lsl ) : t -> int -> t
    val ( lsr ) : t -> int -> t

  end

end

module UInt8 : sig

  type t = uint8 [@@immediate]

  include S with type t := t

  val of_uint16_trunc : uint16 -> t
  val of_uint32_trunc : uint32 -> t
  val of_uint64_trunc : uint64 -> t

  val to_uint16 : t -> uint16
  val to_uint32 : t -> uint32
  val to_uint64 : t -> uint64
  val to_int : t -> int

end

module UInt16 : sig

  type t = uint16 [@@immediate]

  include S with type t := t

  val of_uint8 : uint8 -> t
  val of_uint32_trunc : uint32 -> t
  val of_uint64_trunc : uint64 -> t

  val to_uint32 : t -> uint32
  val to_uint64 : t -> uint64
  val to_int : t -> int
  val to_uint8_trunc : t -> uint8
  val to_uint8_opt : t -> uint8 option

end

module UInt32 : sig

  type t = uint32 (* [@@immediate64] *)

  include S with type t := t

  val of_bits : int32 -> t
  val to_bits : t -> int32

  val of_uint8 : uint8 -> t
  val of_uint16 : uint16 -> t
  val of_uint64_trunc : uint64 -> t

  val to_uint64 : t -> uint64
  val to_uint8_trunc : t -> uint8
  val to_uint16_trunc : t -> uint8
  val to_uint8_opt : t -> uint8 option
  val to_uint16_opt : t -> uint16 option
  val to_int_opt : t -> int option

end

module UInt64 : sig

  type t = uint64

  include S with type t := t

  val of_bits : int64 -> t
  val to_bits : t -> int64

  val of_uint8 : uint8 -> t
  val of_uint16 : uint16 -> t
  val of_uint32 : uint32 -> t

  val to_uint8_trunc : t -> uint8
  val to_uint16_trunc : t -> uint8
  val to_uint32_trunc : t -> uint32
  val to_uint8_opt : t -> uint8 option
  val to_uint16_opt : t -> uint16 option
  val to_uint32_opt : t -> uint32 option
  val to_int_opt : t -> int option

end
