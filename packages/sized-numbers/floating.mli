(** Floating point numbers *)

type float32 = private float (* [@@immediate64] *)
type float64 = float

module type S = sig

  type t

  val of_int : int -> t
  val to_int : t -> int

  val of_float : float -> t
  val to_float : t -> float

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

    val ( = ) : t -> t -> bool
    val ( <> ) : t -> t -> bool
    val ( < ) : t -> t -> bool
    val ( > ) : t -> t -> bool
    val ( <= ) : t -> t -> bool
    val ( >= ) : t -> t -> bool

  end

end

module Float32 : sig

  type t = float32 (* [@@immediate64] *)

  include S with type t := t

end

module Float64 : sig

  type t = float64

  include module type of Float with type t := t
  include S with type t := t

end
