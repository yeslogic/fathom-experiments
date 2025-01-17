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
  let trunc x = (x lsl shift) asr shift

  let neg x = trunc (-x)
  let add x y = trunc (x + y)
  let sub x y = trunc (x - y)
  let mul x y = trunc (x * y)
  let div = Int.div
  let rem = Int.rem
  let succ x = trunc (Int.succ x)
  let pred x = trunc (Int.pred x)
  let abs = Int.abs
  let logand = Int.logand
  let logor = Int.logor
  let logxor = Int.logxor
  let lognot = Int.lognot
  let shift_left x y = trunc (x lsl y)
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
  let trunc x = (x lsl shift) asr shift

  let neg x = trunc (-x)
  let add x y = trunc (x + y)
  let sub x y = trunc (x - y)
  let mul x y = trunc (x * y)
  let div = Int.div
  let rem = Int.rem
  let succ x = trunc (Int.succ x)
  let pred x = trunc (Int.pred x)
  let abs = Int.abs
  let logand = Int.logand
  let logor = Int.logor
  let logxor = Int.logxor
  let lognot = Int.lognot
  let shift_left x y = trunc (x lsl y)
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

  type t = int32

  let num_bits = 32
  let num_bytes = 4
  let zero = Int32.zero
  let one = Int32.one
  let minus_one = Int32.minus_one
  let min_int = Int32.min_int
  let max_int = Int32.max_int

  let neg = Int32.neg
  let add = Int32.add
  let sub = Int32.sub
  let mul = Int32.mul
  let div = Int32.div
  let rem = Int32.rem
  let succ = Int32.succ
  let pred = Int32.pred
  let abs = Int32.abs
  let logand = Int32.logand
  let logor = Int32.logor
  let logxor = Int32.logxor
  let lognot = Int32.lognot
  let shift_left = Int32.shift_left
  let shift_right = Int32.shift_right
  let shift_right_logical = Int32.shift_right_logical

  let equal = Int32.equal
  let compare = Int32.compare
  let min = Int32.min
  let max = Int32.max

  let of_string = Int32.of_string
  let of_string_opt = Int32.of_string_opt
  let to_string = Int32.to_string

  let pp ppf x = Format.pp_print_string ppf (to_string x)

  let seeded_hash = Int32.seeded_hash
  let hash = Int32.hash

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

  let of_int8 = Int32.of_int
  let of_int16 = Int32.of_int
  let of_int = Int32.of_int

  let to_int_trunc = Int32.to_int

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

  type t = int64

  let num_bits = 64
  let num_bytes = 8
  let zero = Int64.zero
  let one = Int64.one
  let minus_one = Int64.minus_one
  let min_int = Int64.min_int
  let max_int = Int64.max_int

  let neg = Int64.neg
  let add = Int64.add
  let sub = Int64.sub
  let mul = Int64.mul
  let div = Int64.div
  let rem = Int64.rem
  let succ = Int64.succ
  let pred = Int64.pred
  let abs = Int64.abs
  let logand = Int64.logand
  let logor = Int64.logor
  let logxor = Int64.logxor
  let lognot = Int64.lognot
  let shift_left = Int64.shift_left
  let shift_right = Int64.shift_right
  let shift_right_logical = Int64.shift_right_logical

  let equal = Int64.equal
  let compare = Int64.compare
  let min = Int64.min
  let max = Int64.max

  let of_string = Int64.of_string
  let of_string_opt = Int64.of_string_opt
  let to_string = Int64.to_string

  let pp ppf x = Format.pp_print_string ppf (to_string x)

  let seeded_hash = Int64.seeded_hash
  let hash = Int64.hash

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

  let of_int8 = Int64.of_int
  let of_int16 = Int64.of_int
  let of_int32 = Int64.of_int32
  let of_int = Int64.of_int

  let to_int32_trunc = Int64.to_int32
  let to_int_trunc = Int64.to_int

  let to_int_opt =
    let min_int = of_int Int.min_int in
    let max_int = of_int Int.max_int in
    fun x ->
      if min_int <= x && x <= max_int then
        Some (to_int_trunc x)
      else
        None

end
