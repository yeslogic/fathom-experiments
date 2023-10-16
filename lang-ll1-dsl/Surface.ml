type tm =
  | Empty
  | Name of string
  | Int of int
  | Range of bound * bound
  | Not of tm
  | Cat of tm * tm
  | Alt of tm * tm
  | Action of tm * (string * tm)

and bound =
  | Open
  | Inclusive of tm
  | Exclusive of tm

let empty = Empty
let name n = Name n
let int i = Int i
let range start stop = Range (start, stop)
let not t = Not t
let cat t0 t1 = Cat (t0, t1)
let alt t0 t1 = Alt (t0, t1)
let action t0 (n, t1) = Action (t0, (n, t1))

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

  type item_context =
    (string * Core.Refiner.item_var) list

  let empty_item_context = []

  let byte_of_int i =
    if 0 <= i && i <= 255 then
      Char.chr i
    else
      failwith ("error: integer `" ^ string_of_int i ^ "` is outside the range `0..255`")

  let byte_set_of_int i =
    ByteSet.singleton (byte_of_int i)

  let byte_set_of_range start stop =
    let start =
      match start with
      | Open -> 0
      | Inclusive (Int start) -> start (* TODO: Check [start] is in 0..255 *)
      | Exclusive (Int start) -> start + 1 (* TODO: Check [start] is in 0>..255 *)
      | _ -> failwith "error: integer literal expected"
    in
    let stop =
      match stop with
      | Open -> 255
      | Inclusive (Int stop) -> stop (* TODO: Check [stop] is in 0..255 *)
      | Exclusive (Int stop) -> stop - 1 (* TODO: Check [stop] is in 0..<255 *)
      | _ -> failwith "error: integer literal expected"
    in
    ByteSet.range (Char.chr start) (Char.chr stop)

  let rec elab_expr items locals t : Core.Refiner.synth_ty =
    match t with
    | Empty -> Core.Refiner.Unit.intro
    | Name name -> begin
        match List.assoc_opt name locals with
        | Some var -> Core.Refiner.Structural.local var
        | None -> failwith ("error: unbound variable `" ^ name ^ "`") (* TODO: improve diagnostics *)
    end
    | Int i ->
        Core.Refiner.Byte.intro (byte_of_int i)
    | Cat (t0, t1) ->
        Core.Refiner.Pair.intro
          (elab_expr items locals t0)
          (elab_expr items locals t1)
    | _ -> failwith "TODO"

  let rec elab_format items t : Core.Refiner.is_format =
    match t with
    | Empty -> Core.Refiner.Format.empty
    | Name name -> begin
        match List.assoc_opt name items with
        | Some var -> Core.Refiner.Format.item var
        | None -> failwith ("error: unbound variable `" ^ name ^ "`") (* TODO: improve diagnostics *)
    end
    | Int i -> Core.Refiner.Format.byte (byte_set_of_int i)
    | Range (start, stop) -> Core.Refiner.Format.byte (byte_set_of_range start stop)
    | Not (Int i) -> Core.Refiner.Format.byte (byte_set_of_int i |> ByteSet.neg)
    | Not (Range (start, stop)) -> Core.Refiner.Format.byte (byte_set_of_range start stop |> ByteSet.neg)
    | Not _ -> failwith "error: Can only apply `!_` to bytes and byte ranges" (* TODO: improve diagnostics *)
    | Cat (t0, t1) -> begin
        try
          Core.Refiner.Format.cat (elab_format items t0) (elab_format items t1)
        with
        | Core.Refiner.Format.AmbiguousConcatenation ->
            failwith "error: ambiguous concatenation" (* TODO: improve diagnostics *)
    end
    | Alt (t0, t1) -> begin
        try
          Core.Refiner.Format.alt (elab_format items t0) (elab_format items t1)
        with
        | Core.Refiner.Format.AmbiguousAlternation ->
            failwith "error: ambiguous alternation" (* TODO: improve diagnostics *)
    end
    | Action (f, (name, e)) ->
        Core.Refiner.Format.map
          (name, fun x -> elab_expr items [name, x] e)
          (elab_format items f)

  let elab_program items program : Core.Refiner.is_program =
    let rec go items =
      function
      | [] -> Core.Refiner.Program.empty
      | (name, format) :: rest ->
          Core.Refiner.Program.def_format (name, elab_format items format)
            (fun var -> go ((name, var) :: items) rest)
            (* FIXME:   ^^ tailcall? *)
    in
    go items program.items

end

let elab_program p =
  Elab.elab_program Elab.empty_item_context p
