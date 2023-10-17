type bound =
  | Inclusive of int
  | Exclusive of int

type range = {
  start : bound option;
  stop : bound option;
}

type tm =
  | Empty
  | Name of string
  | Byte of int
  | ByteRange of range
  | Not of tm
  | Cat of tm * tm
  | Alt of tm * tm

let empty = Empty
let name n = Name n
let byte i = Byte i
let byte_range start stop = ByteRange { start; stop }
let not t = Not t
let cat t0 t1 = Cat (t0, t1)
let alt t0 t1 = Alt (t0, t1)

type program = {
  items : (string * tm) list;
}

let program items =
  { items }


module Elab : sig
  (** Elaboration of the surface language into the core language. *)

  type item_context

  val empty_item_context : item_context

  val elab_program : item_context -> program -> Core.Refiner.is_program

end = struct

  type item_context = (string * Core.Refiner.item_var) list

  let empty_item_context = []

  let byte_set_of_int i =
    if 0 <= i && i <= 255 then
      ByteSet.singleton (Char.chr i)
    else
      failwith ("error: integer `" ^ string_of_int i ^ "` is outside the range `0..255`")

  let byte_set_of_range r =
    let start =
      match r.start with
      | None -> 0
      | Some (Inclusive start) -> start (* TODO: Check [start] is in 0..255 *)
      | Some (Exclusive start) -> start + 1 (* TODO: Check [start] is in 0>..255 *)
    in
    let stop =
      match r.stop with
      | None -> 255
      | Some (Inclusive stop) -> stop (* TODO: Check [stop] is in 0..255 *)
      | Some (Exclusive stop) -> stop - 1 (* TODO: Check [stop] is in 0..<255 *)
    in
    ByteSet.range (Char.chr start) (Char.chr stop)

  let rec elab_format context tm : Core.Refiner.is_format =
    match tm with
    | Empty -> Core.Refiner.Format.empty
    | Name name -> begin
        match List.assoc_opt name context with
        | Some var -> Core.Refiner.Format.item var
        | None -> failwith ("error: unbound variable `" ^ name ^ "`") (* TODO: improve diagnostics *)
    end
    | Byte i -> Core.Refiner.Format.byte (byte_set_of_int i)
    | ByteRange r -> Core.Refiner.Format.byte (byte_set_of_range r)
    | Not (Byte i) -> Core.Refiner.Format.byte (byte_set_of_int i |> ByteSet.neg)
    | Not (ByteRange r) -> Core.Refiner.Format.byte (byte_set_of_range r |> ByteSet.neg)
    | Not _ -> failwith "error: Can only apply `!_` to bytes and byte ranges" (* TODO: improve diagnostics *)
    | Cat (t0, t1) -> begin
        try
          Core.Refiner.Format.cat (elab_format context t0) (elab_format context t1)
        with
        | Core.Refiner.Format.AmbiguousConcatenation ->
            failwith "error: ambiguous concatenation" (* TODO: improve diagnostics *)
    end
    | Alt (t0, t1) -> begin
        try
          Core.Refiner.Format.alt (elab_format context t0) (elab_format context t1)
        with
        | Core.Refiner.Format.AmbiguousAlternation ->
            failwith "error: ambiguous alternation" (* TODO: improve diagnostics *)
    end


  let elab_program context program : Core.Refiner.is_program =
    let rec go context =
      function
      | [] -> Core.Refiner.Program.empty
      | (name, format) :: items ->
          Core.Refiner.Program.def_format (name, elab_format context format)
            (fun var -> go ((name, var) :: context) items)
    in
    go context program.items

end

let elab_program p =
  Elab.elab_program Elab.empty_item_context p
