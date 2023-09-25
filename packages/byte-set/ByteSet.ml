type t = bytes
(** Each bit in the array represents a number in the range [0 .. 255]. *)

let size = 256 / 8

let make f =
  Bytes.init size (fun i ->
    let b0 = Bool.to_int (f (Char.chr (i * 8 + 0))) lsl 0 in
    let b1 = Bool.to_int (f (Char.chr (i * 8 + 1))) lsl 1 in
    let b2 = Bool.to_int (f (Char.chr (i * 8 + 2))) lsl 2 in
    let b3 = Bool.to_int (f (Char.chr (i * 8 + 3))) lsl 3 in
    let b4 = Bool.to_int (f (Char.chr (i * 8 + 4))) lsl 4 in
    let b5 = Bool.to_int (f (Char.chr (i * 8 + 5))) lsl 5 in
    let b6 = Bool.to_int (f (Char.chr (i * 8 + 6))) lsl 6 in
    let b7 = Bool.to_int (f (Char.chr (i * 8 + 7))) lsl 7 in
    Char.chr (b7 lor b6 lor b5 lor b4 lor b3 lor b2 lor b1 lor b0))

let empty = make (fun _ -> false)
let full = make (fun _ -> true)
let singleton c = make (fun c' -> c = c')
let range start stop = make (fun c -> start <= c && c <= stop)

let mem c s =
  let b = Char.code c / 8 in
  let i = Char.code c mod 8 in
  let w = Char.code (Bytes.get s b) in
  Int.shift_left 1 i land w > 0

let union s1 s2 = make (fun c -> mem c s1 || mem c s2)
let inter s1 s2 = make (fun c -> mem c s1 && mem c s2)
let neg s = make (fun c -> not (mem c s))

let equal = Bytes.equal
let is_empty = equal empty
let disjoint s1 s2 = is_empty (inter s1 s2)

let of_string str = make (String.contains str)
let of_bytes buf = make (Bytes.contains buf)

let elements s =
  Seq.init 256 Char.chr
  |> Seq.filter (fun c -> mem c s)

let ranges s : (char * char) Seq.t =
  let[@tail_mod_cons] rec go range (cs : _ Seq.t) () : _ Seq.node =
    match cs (), range with
    | Nil, None -> Nil
    | Nil, Some range -> Cons (range, Seq.empty)
    | Cons (c, next), None -> (go [@tailcall]) (Some (c, c)) next ()
    | Cons (c, next), Some (r0, r1) ->
        begin match Char.code c - Char.code r1 with
        | 1 ->  (go [@tailcall]) (Some (r0, c)) next ()
        | _ -> Cons ((r0, r1), go None next)
        end
  in
  go None (elements s)

let pp_print ppf s =
  let pp_sep ppf () = Format.fprintf ppf ",@ " in
  let pp_print_range ppf (c0, c1) =
    let r0, r1 = Char.(code c0, code c1) in
    match r1 - r0 with
    | 0 -> Format.fprintf ppf "%i" r0
    | 1 -> Format.fprintf ppf "%i,@ %i" r0 r1
    | _ -> Format.fprintf ppf "%i..%i" r0 r1
  in
  Format.fprintf ppf "{%a}"
    (Format.pp_print_seq ~pp_sep pp_print_range) (ranges s)
