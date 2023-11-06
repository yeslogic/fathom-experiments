type tm =
  | Empty
  | Name of string
  | Int of int
  | Range of bound * bound
  | Not of tm
  | Seq of tm * tm
  | Union of tm * tm
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
let seq t0 t1 = Seq (t0, t1)
let union t0 t1 = Union (t0, t1)
let action t0 (n, t1) = Action (t0, (n, t1))

type program = {
  items : (string * tm option * tm) list;
}

let program items =
  { items }


module Elab : sig
  (** Elaboration of the surface language into the core language. *)

  type item_context

  val empty_item_context : item_context

  val elab_program : item_context -> program -> Basis.Void.t Core.Refiner.is_program

end = struct

  module R = Core.Refiner
  module Void = Basis.Void

  type item_context =
    (string * R.item_var) list

  let empty_item_context = []

  type local_context =
    (string * R.local_var) list

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

  let rec elab_expr (items : item_context) (locals : local_context) (t : tm) : Void.t R.synth_ty =
    match t with
    | Empty -> R.Unit.intro
    (* TODO: Handle bultins more systematically *)
    | Name ("Format" | "Type" | "U8" as name)
        when items |> List.assoc_opt name |> Option.is_none
          && locals |> List.assoc_opt name |> Option.is_none ->
        failwith "error: expression expected"
    | Name name -> begin
        match List.assoc_opt name locals with
        | Some var -> R.Structural.local var
        | None -> failwith ("error: unbound variable `" ^ name ^ "`") (* TODO: improve diagnostics *)
    end
    | Int i ->
        R.Byte.intro (byte_of_int i)
    | Seq (t0, t1) ->
        R.Pair.intro
          (elab_expr items locals t0)
          (elab_expr items locals t1)
    | _ -> failwith "TODO"

  let rec elab_format (items : item_context) (t : tm) : Void.t R.is_format =
    match t with
    | Empty -> R.Format.empty
    (* TODO: Handle bultins more systematically *)
    | Name ("Format" | "Type" | "U8" as name)
        when items |> List.assoc_opt name |> Option.is_none ->
        failwith "error: format expected"
    | Name name -> begin
        match List.assoc_opt name items with
        | Some var -> R.Format.item var
        | None -> R.Format.fail (failwith ("error: unbound variable `" ^ name ^ "`")) (* TODO: improve diagnostics *)
    end
    | Int i -> R.Format.byte (byte_set_of_int i)
    | Range (start, stop) -> R.Format.byte (byte_set_of_range start stop)
    | Not (Int i) -> R.Format.byte (byte_set_of_int i |> ByteSet.neg)
    | Not (Range (start, stop)) -> R.Format.byte (byte_set_of_range start stop |> ByteSet.neg)
    | Not _ -> R.Format.fail (failwith "error: Can only apply `!_` to bytes and byte ranges") (* TODO: improve diagnostics *)
    | Seq (t0, t1) ->
        R.Format.seq (elab_format items t0) (elab_format items t1)
        |> R.ItemM.handle (function
            (* TODO: improve diagnostics *)
            | `AmbiguousFormat -> R.Format.fail (failwith "error: ambiguous concatenation"))
    | Union (t0, t1) ->
        R.Format.union (elab_format items t0) (elab_format items t1)
        |> R.ItemM.handle (function
            (* TODO: improve diagnostics *)
            | `AmbiguousFormat -> R.Format.fail (failwith "error: ambiguous alternation")
            | `ReprMismatch (_, _) -> R.Format.fail (failwith "error: mismatched represenations"))
    | Action (f, (name, e)) ->
        R.Format.map
          (name, fun x -> elab_expr items [name, x] e)
          (elab_format items f)

  let rec elab_ann (items : item_context) (t : tm) : [`FormatUniv | `TypeUniv | `Type of Void.t R.is_ty] =
    match t with
    (* TODO: Handle bultins more systematically *)
    | Name "Format" when items |> List.assoc_opt "Format" |> Option.is_none -> `FormatUniv
    | Name "Type" when items |> List.assoc_opt "Type" |> Option.is_none -> `TypeUniv
    | Name "U8" when items |> List.assoc_opt "U8" |> Option.is_none -> `Type R.Byte.form
    | Empty -> `Type R.Unit.form
    | Seq (t0, t1) -> begin
        match elab_ann items t0, elab_ann items t1 with
        | `Type t0, `Type t1 -> `Type (R.Pair.form t0 t1)
        | _, _ -> failwith "error: type expected"
    end
    | _ -> failwith "error: invalid annotation"

  let elab_program (items : item_context) (p : program) : Void.t R.is_program =
    let rec go items =
      function
      | [] -> R.Program.empty
      | (name, None, format) :: rest ->
          R.Program.def_format (name, elab_format items format)
            (fun var -> go ((name, var) :: items) rest)
            (* FIXME:   ^^ tailcall? *)
      | (name, Some ann, format) :: rest -> begin
          match elab_ann items ann with
          | `FormatUniv ->
              R.Program.def_format (name, elab_format items format)
                (fun var -> go ((name, var) :: items) rest)
                (* FIXME:   ^^ tailcall? *)
          | `TypeUniv -> failwith "error: type definitions are not yet supported"
          | `Type _ -> failwith "error: term definitions are not yet supported"
      end
    in
    go items p.items

end

let elab_program p =
  Elab.elab_program Elab.empty_item_context p
  |> Core.Refiner.ItemM.run
  |> Result.fold ~ok:Fun.id ~error:Basis.Void.absurd
