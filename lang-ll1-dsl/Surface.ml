type tm =
  | Empty
  | Name of string
  | Int of int
  | Range of bound * bound
  | Not of tm
  | Seq of tm * tm
  | Union of tm * tm
  | Alt of tm * tm
  | Action of tm * (string * tm)
  | Proj of tm * string

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
let alt t0 t1 = Alt (t0, t1)
let action t0 (n, t1) = Action (t0, (n, t1))
let proj t l = Proj (t, l)

type program = {
  items : (string * tm option * tm) list;
}

let program items =
  { items }


module Elab : sig
  (** Elaboration of the surface language into the core language. *)

  module ItemContext : sig

    type t

    val empty : t

  end

  val elab_program : ItemContext.t -> program -> Basis.Void.t Core.Refiner.is_program

end = struct

  module R = Core.Refiner
  module Void = Basis.Void

  module ItemContext = struct

    type t = {
      items : (string * R.item_var) list;
    }

    let empty = {
      items = [];
    }

    let def_item (n, var : string * R.item_var) (context : t) : t =
      { items = (n, var) :: context.items }

    type entry = [
      | `FormatUniv
      | `TypeUniv
      | `Type of Void.t R.is_ty
      | `Item of R.item_var
    ]

    let lookup (n : string) (context : t) : [> entry] option =
      match List.assoc_opt n context.items with
      | Some var -> Some (`Item var)
      | None -> begin
          match n with
          | "Format" -> Some `FormatUniv
          | "Type" -> Some `TypeUniv
          | "U8" -> Some (`Type R.Byte.form)
          | _ -> None
      end

  end

  module LocalContext = struct

    type t = {
      items : (string * R.item_var) list;
      locals : (string * R.local_var) list;
    }

    let to_item_context (context : t) : ItemContext.t =
      { items = context.items }

    let of_item_context (context : ItemContext.t) : t =
      { items = context.items; locals = [] }

    let bind_local (n, var : string * R.local_var) (context : t) : t =
      { context with locals = (n, var) :: context.locals }

    type entry = [
      | ItemContext.entry
      | `Local of R.local_var
    ]

    let lookup (n : string) (context : t) : [> entry] option =
      match List.assoc_opt n context.locals with
      | Some var -> Some (`Local var)
      | None -> ItemContext.lookup n (to_item_context context)

  end

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

  let rec elab_expr (context : LocalContext.t) (t : tm) : Void.t R.synth_ty =
    match t with
    | Empty ->
        R.Unit.intro
    | Name name -> begin
        match LocalContext.lookup name context with
        | Some (`Local var) ->
            R.Structural.local var
            |> R.LocalM.handle (function
                (* TODO: improve diagnostics *)
                | `UnboundVariable -> failwith "bug: unbound local variable")
        | Some (`FormatUniv | `TypeUniv | `Type _ | `Item _) -> failwith "error: local variable expected"
        | None -> failwith ("error: unbound variable `" ^ name ^ "`") (* TODO: improve diagnostics *)
    end
    | Int i ->
        R.Byte.intro (byte_of_int i)
    | Seq (t0, t1) ->
        R.Pair.intro (elab_expr context t0) (elab_expr context t1)
    | _ ->
        failwith "error: expression expected"

  let rec elab_format (context : ItemContext.t) (t : tm) : Void.t R.is_format =
    match t with
    | Empty ->
        R.Format.empty
    | Name name -> begin
        match ItemContext.lookup name context with
        | Some (`Item var) ->
            R.Format.item var
            |> R.ItemM.handle (function
                (* TODO: improve diagnostics *)
                | `FormatExpected -> R.Format.fail (failwith "error: format expected")
                | `UnboundVariable -> R.Format.fail (failwith "bug: unbound item variable"))
        | Some (`FormatUniv | `TypeUniv | `Type _) ->  R.Format.fail (failwith "error: format expected") (* TODO: improve diagnostics *)
        | None -> R.Format.fail (failwith ("error: unbound variable `" ^ name ^ "`")) (* TODO: improve diagnostics *)
    end
    | Int i ->
        R.Format.byte (byte_set_of_int i)
    | Range (start, stop) ->
        R.Format.byte (byte_set_of_range start stop)
    | Not (Int i) ->
        R.Format.byte (byte_set_of_int i |> ByteSet.neg)
    | Not (Range (start, stop)) ->
        R.Format.byte (byte_set_of_range start stop |> ByteSet.neg)
    | Not _ ->
        R.Format.fail (failwith "error: Can only apply `!_` to bytes and byte ranges") (* TODO: improve diagnostics *)
    | Seq (t0, t1) ->
        R.Format.seq (elab_format context t0) (elab_format context t1)
        |> R.ItemM.handle (function
            (* TODO: improve diagnostics *)
            | `AmbiguousFormat -> R.Format.fail (failwith "error: ambiguous sequence"))
    | Union (t0, t1) ->
        R.Format.union (elab_format context t0) (elab_format context t1)
        |> R.ItemM.handle (function
            (* TODO: improve diagnostics *)
            | `AmbiguousFormat -> R.Format.fail (failwith "error: ambiguous union")
            | `ReprMismatch (_, _) -> R.Format.fail (failwith "error: mismatched represenations"))
    | Alt (t0, t1) ->
        R.Format.alt (elab_format context t0) (elab_format context t1)
        |> R.ItemM.handle (function
            (* TODO: improve diagnostics *)
            | `AmbiguousFormat -> R.Format.fail (failwith "error: ambiguous alternation")
            | `ReprMismatch (_, _) -> R.Format.fail (failwith "error: mismatched represenations"))
    | Action (f, (name, e)) ->
        R.Format.map
          (name, fun x -> elab_expr LocalContext.(of_item_context context |> bind_local (name, x)) e)
          (elab_format context f)
    | Proj (_, _) ->
        failwith "TODO"

  let rec elab_ty (context : ItemContext.t) (t : tm) : Void.t R.is_ty =
    match t with
    | Empty ->
        R.Unit.form
    | Name name -> begin
        match ItemContext.lookup name context with
        | Some (`Item var) ->
            R.Structural.item_ty var
            |> R.ItemM.handle (function
                (* TODO: improve diagnostics *)
                | `TypeExpected -> failwith "error: type expected"
                | `UnboundVariable -> failwith "bug: unbound item variable")
        | Some (`Type t) -> t
        | Some (`FormatUniv | `TypeUniv) -> failwith "error: type expected" (* TODO: improve diagnostics *)
        | None -> failwith ("error: unbound variable `" ^ name ^ "`") (* TODO: improve diagnostics *)
    end
    | Seq (t0, t1) ->
        R.Pair.form (elab_ty context t0) (elab_ty context t1)
    | Proj (t, "Repr") ->
        R.Format.repr (elab_format context t)
    | _ ->
        failwith "error: type expected"

  let elab_ann (context : ItemContext.t) (t : tm) : [`FormatUniv | `TypeUniv | `Type of Void.t R.is_ty] =
    match t with
    | Name name -> begin
        match ItemContext.lookup name context with
        | Some (`FormatUniv | `TypeUniv | `Type _ as i) -> i
        | Some (`Item _) -> failwith "error: invalid annotation"
        | None -> failwith ("error: unbound variable `" ^ name ^ "`")
    end
    | Empty ->
        `Type R.Unit.form
    | Seq (t0, t1) -> begin
        `Type (R.Pair.form (elab_ty context t0) (elab_ty context t1))
    end
    | _ ->
        failwith "error: invalid annotation"

  let elab_program (context : ItemContext.t) (p : program) : Void.t R.is_program =
    let rec go context =
      function
      | [] ->
          R.Program.empty
      | (name, None, t) :: rest ->
          R.Program.def_format (name, elab_format context t)
            (fun var -> go (ItemContext.def_item (name, var) context) rest)
            (* FIXME:   ^^ tailcall? *)
      | (name, Some ann, t) :: rest -> begin
          match elab_ann context ann with
          | `FormatUniv ->
              R.Program.def_format (name, elab_format context t)
                (fun var -> go (ItemContext.def_item (name, var) context) rest)
                (* FIXME:   ^^ tailcall? *)
          | `TypeUniv ->
              R.Program.def_ty (name, elab_ty context t)
                (fun var -> go (ItemContext.def_item (name, var) context) rest)
                (* FIXME:   ^^ tailcall? *)
          | `Type _ -> failwith "error: term definitions are not yet supported"
      end
    in
    go context p.items

end

let elab_program p =
  Elab.elab_program Elab.ItemContext.empty p
  |> Core.Refiner.ItemM.run
  |> Result.fold ~ok:Fun.id ~error:Basis.Void.absurd
