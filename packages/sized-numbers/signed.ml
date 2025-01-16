type int8 = int
type int16 = int
type nonrec int32 = int32
type nonrec int64 = int64

module type S = sig

  include Unsigned.S

  val minus_one : t
  val abs : t -> t
  val shift_right_logical : t -> int -> t

end

module Int8 = struct

  type t = int8

  let num_bits = 8
  let num_bytes = 1
  let zero = 0
  let one = 1
  let minus_one = -1
  let min_int = -128 (* -2^(8 - 1) *)
  let max_int = 127 (* 2^(8 - 1) - 1 *)

  let shift = Sys.int_size - num_bits
  let wrap_bounds x = (x lsl shift) asr shift

  let neg x = wrap_bounds (-x)
  let add x y = wrap_bounds (x + y)
  let sub x y = wrap_bounds (x - y)
  let mul x y = wrap_bounds (x * y)
  let div = Int.div
  let rem = Int.rem
  let succ x = wrap_bounds (Int.succ x)
  let pred x = wrap_bounds (Int.pred x)
  let abs = Int.abs
  let logand = Int.logand
  let logor = Int.logor
  let logxor = Int.logxor
  let lognot = Int.lognot
  let shift_left x y = wrap_bounds (x lsl y)
  let shift_right = Int.shift_right
  let shift_right_logical x y = ((x lsl shift) lsr y) asr shift

  let equal = Stdlib.( = )
  let compare = Int.compare
  let min = Int.min
  let max = Int.max

  let of_string s =
    match int_of_string s with
    | x when x < min_int || x > max_int -> failwith "Int8.of_string"
    | exception Failure _ -> failwith "Int8.of_string"
    | x -> x

  let of_string_opt s = try Some (of_string s) with Failure _ -> None
  let to_string = Int.to_string

  let pp ppf x = Format.pp_print_string ppf (to_string x)

  let seeded_hash = Int.seeded_hash
  let hash = Int.hash

  module O = struct

    let ( ~- ) = neg
    let ( ~+ ) x = x

    let ( + ) = add
    let ( - ) = sub
    let ( * ) = mul
    let ( / ) = div

    let ( = ) = equal
    let ( <> ) x y = compare x y <> 0
    let ( < ) x y = compare x y < 0
    let ( > ) x y = compare x y > 0
    let ( <= ) x y = compare x y <= 0
    let ( >= ) x y = compare x y >= 0

    let ( land ) = logand
    let ( lor ) = logor
    let ( lxor ) = logxor
    let ( lsl ) = shift_left
    let ( lsr ) = shift_right_logical

  end

  let to_int x = x

end

module Int16 = struct

  type t = int16

  let num_bits = 16
  let num_bytes = 2
  let zero = 0
  let one = 1
  let minus_one = -1
  let min_int = -32768 (* -2^(16 - 1) *)
  let max_int = 32767 (* 2^(16 - 1) - 1 *)

  let shift = Sys.int_size - num_bits
  let wrap_bounds x = (x lsl shift) asr shift

  let neg x = wrap_bounds (-x)
  let add x y = wrap_bounds (x + y)
  let sub x y = wrap_bounds (x - y)
  let mul x y = wrap_bounds (x * y)
  let div = Int.div
  let rem = Int.rem
  let succ x = wrap_bounds (Int.succ x)
  let pred x = wrap_bounds (Int.pred x)
  let abs = Int.abs
  let logand = Int.logand
  let logor = Int.logor
  let logxor = Int.logxor
  let lognot = Int.lognot
  let shift_left x y = wrap_bounds (x lsl y)
  let shift_right = Int.shift_right
  let shift_right_logical x y = ((x lsl shift) lsr y) asr shift

  let equal = Stdlib.( = )
  let compare = Int.compare
  let min = Int.min
  let max = Int.max

  let of_string s =
    match int_of_string s with
    | x when x < min_int || x > max_int -> failwith "Int16.of_string"
    | exception Failure _ -> failwith "Int16.of_string"
    | x -> x

  let of_string_opt s = try Some (of_string s) with Failure _ -> None
  let to_string = Int.to_string

  let pp ppf x = Format.pp_print_string ppf (to_string x)

  let seeded_hash = Int.seeded_hash
  let hash = Int.hash

  module O = struct

    let ( ~- ) = neg
    let ( ~+ ) x = x

    let ( + ) = add
    let ( - ) = sub
    let ( * ) = mul
    let ( / ) = div

    let ( = ) = equal
    let ( <> ) x y = compare x y <> 0
    let ( < ) x y = compare x y < 0
    let ( > ) x y = compare x y > 0
    let ( <= ) x y = compare x y <= 0
    let ( >= ) x y = compare x y >= 0

    let ( land ) = logand
    let ( lor ) = logor
    let ( lxor ) = logxor
    let ( lsl ) = shift_left
    let ( lsr ) = shift_right_logical

  end

  let of_int8 x = x
  let to_int x = x

end

module Int32 = struct

  include Stdlib.Int32

  let num_bits = 32
  let num_bytes = 4

  let pp ppf x = Format.pp_print_string ppf (to_string x)

  module O = struct

    let ( ~- ) = neg
    let ( ~+ ) x = x

    let ( + ) = add
    let ( - ) = sub
    let ( * ) = mul
    let ( / ) = div

    let ( = ) = equal
    let ( <> ) x y = compare x y <> 0
    let ( < ) x y = compare x y < 0
    let ( > ) x y = compare x y > 0
    let ( <= ) x y = compare x y <= 0
    let ( >= ) x y = compare x y >= 0

    let ( land ) = logand
    let ( lor ) = logor
    let ( lxor ) = logxor
    let ( lsl ) = shift_left
    let ( lsr ) = shift_right_logical

  end

  let of_int8 x = Stdlib.Int32.of_int x
  let of_int16 x = Stdlib.Int32.of_int x
  let of_int x = Stdlib.Int32.of_int x

  let to_int_trunc x = Stdlib.Int32.to_int x

  let to_int_opt =
    let min_int = of_int Int.min_int in
    let max_int = of_int Int.max_int in
    fun x ->
      if min_int <= x && x <= max_int then
        Some (to_int_trunc x)
      else
        None

end

module Int64 = struct

  include Stdlib.Int64

  let num_bits = 64
  let num_bytes = 8

  let pp ppf x = Format.pp_print_string ppf (to_string x)

  module O = struct

    let ( ~- ) = neg
    let ( ~+ ) x = x

    let ( + ) = add
    let ( - ) = sub
    let ( * ) = mul
    let ( / ) = div

    let ( = ) = equal
    let ( <> ) x y = compare x y <> 0
    let ( < ) x y = compare x y < 0
    let ( > ) x y = compare x y > 0
    let ( <= ) x y = compare x y <= 0
    let ( >= ) x y = compare x y >= 0

    let ( land ) = logand
    let ( lor ) = logor
    let ( lxor ) = logxor
    let ( lsl ) = shift_left
    let ( lsr ) = shift_right_logical

  end

  let of_int8 x = Stdlib.Int64.of_int x
  let of_int16 x = Stdlib.Int64.of_int x
  let of_int32 x = Stdlib.Int64.of_int32 x
  let of_int x = Stdlib.Int64.of_int x

  let to_int32_trunc x = Stdlib.Int64.to_int32 x
  let to_int_trunc x = Stdlib.Int64.to_int x

  let to_int_opt =
    let min_int = of_int Int.min_int in
    let max_int = of_int Int.max_int in
    fun x ->
      if min_int <= x && x <= max_int then
        Some (to_int_trunc x)
      else
        None

end
