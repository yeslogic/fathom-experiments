type uint8 = int
type uint16 = int
type uint32 = int32
type uint64 = int64

module Basics = struct

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

  end

end

module Extras = struct

  module type S = sig

    type t

    val min : t -> t -> t
    val max : t -> t -> t

  end

  module Make (B : Basics.S) : S with type t = B.t = struct

    type t = B.t

    let min x y = if compare x y <= 0 then x else y
    let max x y = if compare x y >= 0 then x else y

  end

end

module String_conv = struct

  module type S = sig

    type t

    val of_string : string -> t
    val of_string_opt : string -> t option
    val to_string : t -> string

    val pp : Format.formatter -> t -> unit

  end

  module Make (B : Basics.S) (X : sig

    val to_int : B.t -> int
    val of_int : int -> B.t
    val type_name : string

  end) : S with type t = B.t = struct

    type t = B.t

    open B
    open X

    (* NOTE: We could avoid redefining these if the standard library provided
      support for the following definitions:

      - [Int32.unsigned_min_int]
      - [Int32.unsigned_max_int]
      - [Int32.unsigned_of_string]
      - [Int32.unsigned_to_string]
      - [Int64.unsigned_min_int]
      - [Int64.unsigned_max_int]
      - [Int64.unsigned_of_string]
      - [Int64.unsigned_to_string]
    *)

    let parse_digit (ch : char) : int =
      match ch with
      | '0' .. '9' -> Char.(code ch - code '0')
      | 'A' .. 'F' -> Char.(code ch - code 'A' + 10)
      | 'a' .. 'f' -> Char.(code ch - code 'a' + 10)
      | _ -> -1

    let print_digit (x : int) : char =
      let offset = if x < 10 then '0' else 'a' in
      Char.chr (Char.code offset + x)

    let err_msg = type_name ^ ".of_string"

    let of_string (s : string) : t =
      let pos = ref 0 in

      (* Skip sign and parse base *)
      if !pos >= String.length s then failwith err_msg;
      if s.[!pos] = '+' then
        incr pos;
      let base =
        if !pos + 1 < String.length s && s.[!pos] = '0' then
          match s.[!pos + 1] with
          | 'x' | 'X' -> pos := !pos + 2; 16
          | 'o' | 'O' -> pos := !pos + 2; 8
          | 'b' | 'B' -> pos := !pos + 2; 2
          | 'u' | 'U' -> pos := !pos + 2; 10
          | _ -> 10
        else
          10
      in

      let max_prefix = div max_int (of_int base) in

      (* Parse the first digit *)
      if !pos >= String.length s then failwith err_msg;
      let d = parse_digit s.[!pos] in
      if d < 0 || d >= base then failwith err_msg;
      let res = ref (of_int d) in

      (* Parse the rest of the digits *)
      let rec loop () =
        incr pos;
        if !pos >= String.length s then () else
          let c = s.[!pos] in
          if c = '_' then loop () else
            let d = parse_digit c in
            if d < 0 || d >= base then failwith err_msg;
            if compare !res max_prefix > 0 then failwith err_msg;
            res := add (mul (of_int base) !res) (of_int d);
            if compare !res (of_int d) < 0 then failwith err_msg;
            loop ()
      in
      loop ();

      !res

    let of_string_opt s =
      try Some (of_string s) with
      | Failure _ -> None

    let count_digits (base : int) : t -> int =
      let base = of_int base in
      let rec count_nonzero x count =
        if compare x (of_int 0) <= 0 then count else
          count_nonzero (div x base) (count + 1)
      in
      fun x ->
        if compare x (of_int 0) = 0 then 1 else count_nonzero x 0

    let to_string x =
      let base = of_int 10 in
      let num_digits = count_digits 10 x in
      let buf = Bytes.make num_digits ' ' in
      let rec loop x i =
        let rest = div x base in
        let digit = rem x base in
        Bytes.set buf i (print_digit (to_int digit));
        if i <= 0 then () else
          loop rest (i - 1)
      in
      loop x (num_digits - 1);
      String.of_bytes buf

    let pp ppf x = Format.pp_print_string ppf (to_string x)

  end

end

module Ops = struct

  module type S = sig

    type t

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

  end

  module Make (B : Basics.S) : S with type t = B.t = struct

    type t = B.t

    let ( ~- ) = B.neg
    let ( ~+ ) x = x

    let ( + ) = B.add
    let ( - ) = B.sub
    let ( * ) = B.mul
    let ( / ) = B.div

    let ( = ) = B.equal
    let ( <> ) x y = B.compare x y <> 0
    let ( < ) x y = B.compare x y < 0
    let ( > ) x y = B.compare x y > 0
    let ( <= ) x y = B.compare x y <= 0
    let ( >= ) x y = B.compare x y >= 0

  end

end

module type S = sig

  type t

  include Basics.S with type t := t
  include Extras.S with type t := t
  include String_conv.S with type t := t

  val seeded_hash : int -> t -> int
  val hash : t -> int

  module O : Ops.S with type t := t

end

module UInt8 = struct

  module B = struct

    type t = uint8

    let num_bits = 8
    let num_bytes = 1
    let zero = 0
    let one = 1
    let min_int = 0
    let max_int = 255 (* 2^8 - 1 *)

    let wrap_bounds x = x land max_int

    let neg x = wrap_bounds (-x)
    let add x y = wrap_bounds (x + y)
    let sub x y = wrap_bounds (x - y)
    let mul x y = wrap_bounds (x * y)
    let div = Int.div
    let rem = Int.rem
    let succ x = wrap_bounds (Int.succ x)
    let pred x = wrap_bounds (Int.pred x)
    let logand = Int.logand
    let logor = Int.logor
    let logxor = Int.logxor
    let lognot x = wrap_bounds (Int.lognot x)
    let shift_left x y = wrap_bounds (x lsl y)
    let shift_right = Int.shift_right

    let equal = Stdlib.( = )
    let compare = Stdlib.compare

    let seeded_hash = Int.seeded_hash
    let hash = Int.hash

  end

  include B
  include Extras.Make (B)
  module O = Ops.Make (B)

  let of_string s =
    match int_of_string s with
    | x when x < min_int || x > max_int -> failwith "UInt8.of_string"
    | exception Failure _ -> failwith "UInt8.of_string"
    | x -> x

  let of_string_opt s = try Some (of_string s) with Failure _ -> None
  let to_string = Int.to_string

  let pp ppf x = Format.pp_print_string ppf (to_string x)

  let to_int x = x
  let to_uint16 x = x
  let to_uint32 x = Int32.of_int x
  let to_uint64 x = Int64.of_int x

end

module UInt16 = struct

  module B = struct

    type t = uint16

    let num_bits = 16
    let num_bytes = 2
    let zero = 0
    let one = 1
    (* FIXME: will this work correctly on 32-bit platforms? *)
    let min_int = 0
    let max_int = 65535 (* 2^16 - 1 *)

    let wrap_bounds x = x land max_int

    let neg x = wrap_bounds (-x)
    let add x y = wrap_bounds (x + y)
    let sub x y = wrap_bounds (x - y)
    let mul x y = wrap_bounds (x * y)
    let div = Int.div
    let rem = Int.rem
    let succ x = wrap_bounds (Int.succ x)
    let pred x = wrap_bounds (Int.pred x)
    let logand = Int.logand
    let logor = Int.logor
    let logxor = Int.logxor
    let lognot x = wrap_bounds (Int.lognot x)
    let shift_left x y = wrap_bounds (x lsl y)
    let shift_right = Int.shift_right

    let equal = Stdlib.( = )
    let compare = Stdlib.compare

  end

  include B
  include Extras.Make (B)
  module O = Ops.Make (B)

  let of_string s =
    match int_of_string s with
    | x when x < min_int || x > max_int -> failwith "UInt16.of_string"
    | exception Failure _ -> failwith "UInt16.of_string"
    | x -> x

  let of_string_opt s = try Some (of_string s) with Failure _ -> None
  let to_string = Int.to_string

  let pp ppf x = Format.pp_print_string ppf (to_string x)

  let seeded_hash = Int.seeded_hash
  let hash = Int.hash

  let to_int x = x
  let of_uint8 x = x

  let to_int8_opt =
    fun x -> if O.(x <= UInt8.max_int) then Some x else None

  let to_uint32 x = Int32.of_int x
  let to_uint64 x = Int64.of_int x

end

module UInt32 = struct

  module B = struct

    type t = uint32

    let num_bits = 32
    let num_bytes = 4
    let zero = Int32.zero
    let one = Int32.one
    let min_int = Int32.zero
    let max_int = Int32.minus_one

    let neg = Int32.neg
    let add = Int32.add
    let sub = Int32.sub
    let mul = Int32.mul
    let div = Int32.unsigned_div
    let rem = Int32.unsigned_rem
    let succ = Int32.succ
    let pred = Int32.pred
    let logand = Int32.logand
    let logor = Int32.logor
    let logxor = Int32.logxor
    let lognot = Int32.lognot
    let shift_left = Int32.shift_left
    let shift_right = Int32.shift_right_logical

    let compare = Int32.unsigned_compare
    let equal x y = compare x y = 0

  end

  include B
  include Extras.Make (B)
  module O = Ops.Make (B)

  include String_conv.Make (B) (struct
    let to_int = Int32.to_int
    let of_int = Int32.of_int
    let type_name = "UInt32"
  end)

  let seeded_hash = Int32.seeded_hash
  let hash = Int32.hash

  let of_bits x = x
  let to_bits x = x

  let of_uint8 x = Int32.of_int x
  let of_uint16 x = Int32.of_int x

  let to_int8_opt =
    let max_int = of_uint8 UInt8.max_int in
    fun x -> if O.(x <= max_int) then Some (Int32.to_int x) else None

  let to_int16_opt =
    let max_int = of_uint16 UInt16.max_int in
    fun x -> if O.(x <= max_int) then Some (Int32.to_int x) else None

  let to_int_opt = Int32.unsigned_to_int
  let to_uint64 x = Int64.of_int32 x

end

module UInt64 = struct

  module B = struct

    type t = uint64

    let num_bits = 64
    let num_bytes = 8
    let zero = Int64.zero
    let one = Int64.one
    let min_int = Int64.zero
    let max_int = Int64.minus_one

    let neg = Int64.neg
    let add = Int64.add
    let sub = Int64.sub
    let mul = Int64.mul
    let div = Int64.unsigned_div
    let rem = Int64.unsigned_rem
    let succ = Int64.succ
    let pred = Int64.pred
    let logand = Int64.logand
    let logor = Int64.logor
    let logxor = Int64.logxor
    let lognot = Int64.lognot
    let shift_left = Int64.shift_left
    let shift_right = Int64.shift_right_logical

    let compare = Int64.unsigned_compare
    let equal x y = compare x y = 0

  end

  include B
  include Extras.Make (B)
  module O = Ops.Make (B)

  include String_conv.Make (B) (struct
    let to_int = Int64.to_int
    let of_int = Int64.of_int
    let type_name = "UInt64"
  end)

  let seeded_hash = Int64.seeded_hash
  let hash = Int64.hash

  let of_bits x = x
  let to_bits x = x

  let of_uint8 x = Int64.of_int x
  let of_uint16 x = Int64.of_int x
  let of_uint32 x = Int64.of_int32 x
  let to_int_opt = Int64.unsigned_to_int

end
