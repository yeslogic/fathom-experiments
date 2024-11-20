type float32 = float
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

  val pp : Format.formatter -> t -> unit

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

module Float32 = struct

  type t = float

  let limit_precision x = Int32.(float_of_bits (bits_of_float x))

  let of_int (x : int) : t = Int32.(float_of_bits (of_int x))
  let to_int (x : t) : int = Int32.(to_int (bits_of_float x))

  let of_float (x : float) : t = limit_precision x
  let to_float x = x

  let equal = Float.equal
  let compare = Float.compare
  let min = Float.min
  let max = Float.max

  let of_string s = limit_precision (Float.of_string s)
  let of_string_opt s = try Some (of_string s) with Failure _ -> None
  let to_string = Float.to_string

  let pp ppf x = Format.pp_print_string ppf (to_string x)

  let seeded_hash = Float.seeded_hash
  let hash = Float.hash

  module O = struct

    let ( = ) = equal
    let ( <> ) x y = compare x y <> 0
    let ( < ) x y = compare x y < 0
    let ( > ) x y = compare x y > 0
    let ( <= ) x y = compare x y <= 0
    let ( >= ) x y = compare x y >= 0

  end

end

module Float64 = struct

  include Float

  let of_float x = x
  let to_float x = x

  let pp ppf x = Format.pp_print_string ppf (to_string x)

  module O = struct

    let ( = ) = equal
    let ( <> ) x y = compare x y <> 0
    let ( < ) x y = compare x y < 0
    let ( > ) x y = compare x y > 0
    let ( <= ) x y = compare x y <= 0
    let ( >= ) x y = compare x y >= 0

  end

end
