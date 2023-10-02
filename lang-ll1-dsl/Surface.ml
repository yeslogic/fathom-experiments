type bound =
  | Inclusive of int
  | Exclusive of int

type range = {
  start : bound option;
  stop : bound option;
}

type sign =
  | Pos
  | Neg

type tm =
  | Unit
  | Name of string
  | Byte of sign * int
  | ByteRange of sign * range
  | Cat of tm * tm
  | Alt of tm * tm

type program = {
  items : (string * tm) list;
}

module Elab = struct

  type context = (string * Core.Refiner.item_var) list

  let empty_context = []

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

  let apply_sign sign s =
    match sign with
    | Pos -> s
    | Neg -> ByteSet.neg s

  let rec elab_format context tm : Core.Refiner.is_format =
    match tm with
    | Unit -> Core.Refiner.Format.unit
    | Name name -> begin
        match List.assoc_opt name context with
        | Some var -> Core.Refiner.Format.item var
        | None -> failwith ("error: unbound variable `" ^ name ^ "`")
    end
    | Byte (sign, i) -> Core.Refiner.Format.byte (byte_set_of_int i |> apply_sign sign)
    | ByteRange (sign, r) -> Core.Refiner.Format.byte (byte_set_of_range r |> apply_sign sign)
    | Cat (t0, t1) -> Core.Refiner.Format.cat (elab_format context t0) (elab_format context t1)
    | Alt (t0, t1) -> Core.Refiner.Format.alt (elab_format context t0) (elab_format context t1)

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
