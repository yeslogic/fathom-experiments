(** The start and end position in a source file *)
type loc =
  Lexing.position * Lexing.position

(** Located nodes *)
type 'a located = {
  loc : loc;
  data : 'a;
}

let located (loc : loc) (data : 'a) : 'a located =
  { loc; data }

type tm =
  tm_data located
and tm_data =
  | Empty
  | Name of string
  | Int of int
  | Range of bound * bound
  | Not of tm
  | Seq of tm * tm
  | Union of tm * tm
  | Action of tm * (string * tm)
  | Proj of tm * string
  | RecordEmpty
  | RecordFormat of (string * tm) list
  | RecordType of (string * tm) list
  | RecordLit of (string * tm) list

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
let proj t l = Proj (t, l)

let record_empty = RecordEmpty
let record_format fields = RecordFormat fields
let record_ty fields = RecordType fields
let record_lit fields = RecordLit fields

type program = {
  items : (string * tm option * tm) list;
}

let program items =
  { items }


exception Error of loc * string
exception Bug of loc * string

let error loc msg = raise (Error (loc, msg))
let bug loc msg = raise (Bug (loc, msg))


module Elab : sig
  (** Elaboration of the surface language into the core language. *)

  module ItemContext : sig

    type t

    val empty : t

  end

  val elab_program : ItemContext.t -> program -> Core.Refiner.is_program

end = struct

  module R = Core.Refiner

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
      | `Type of R.is_ty
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

  let byte_of_int loc i =
    if 0 <= i && i <= 255 then
      Char.chr i
    else
      error loc ("integer `" ^ string_of_int i ^ "` is outside the range `0..255`")

  let byte_set_of_int loc i =
    Byte_set.singleton (byte_of_int loc i)

  let byte_set_of_range start stop =
    let start =
      match start with
      | Open -> 0
      | Inclusive { data = Int start; _ } -> start (* TODO: Check [start] is in 0..255 *)
      | Exclusive { data = Int start; _ } -> start + 1 (* TODO: Check [start] is in 0>..255 *)
      | Inclusive { loc; _ } | Exclusive { loc; _ } -> error loc "integer literal expected"
    and stop =
      match stop with
      | Open -> 255
      | Inclusive { data = Int stop; _ } -> stop (* TODO: Check [stop] is in 0..255 *)
      | Exclusive { data = Int stop; _ } -> stop - 1 (* TODO: Check [stop] is in 0..<255 *)
      | Inclusive { loc; _ } | Exclusive { loc; _ } -> error loc "integer literal expected"
    in
    Byte_set.range (Char.chr start) (Char.chr stop)

  let rec elab_check_ty (context : LocalContext.t) (t : tm) : R.check_ty =
    fun ty ->
      match t with
      | _ ->
          R.Structural.conv (elab_synth_ty context t) ty
          |> R.handle_local (function
            | `TypeMismatch (found_ty, expected_ty) ->
                error t.loc
                  (Format.asprintf "type mismatch, found `%a` expected `%a`"
                    Core.pp_print_ty found_ty
                    Core.pp_print_ty expected_ty))
  and elab_synth_ty (context : LocalContext.t) (t : tm) : R.synth_ty =
    match t.data with
    | Empty ->
        R.Unit.intro
    | Name name -> begin
        match LocalContext.lookup name context with
        | Some (`Local var) ->
            R.Structural.local var
            |> R.handle_local (function
                | `UnboundVariable -> bug t.loc "unbound local variable")
        | Some (`Item var) ->
            R.Structural.item_expr var
            |> R.handle_local (function
                | `ExprExpected -> error t.loc "local variable expected"
                | `UnboundVariable -> bug t.loc "unbound local variable")
        | Some (`FormatUniv | `TypeUniv | `Type _) -> error t.loc "local variable expected"
        | None -> error t.loc ("unbound variable `" ^ name ^ "`")
    end
    | Int i ->
        R.Byte.intro (byte_of_int t.loc i)
    | Seq (t0, t1) ->
        R.Pair.intro (elab_synth_ty context t0) (elab_synth_ty context t1)
    | Proj (t, l) ->
        R.Record.proj (elab_synth_ty context t) l
        |> R.handle_local (function
            | `UnknownFieldLabel _ ->
                failwith ("error: unknown field label `" ^ l ^ "`"))
    | RecordEmpty ->
        R.Record.intro_empty
    | RecordLit fields ->
        R.Record.intro (fields |> List.map (fun (l, t) -> l, elab_synth_ty context t))
        |> R.handle_local (function
            | `DuplicateFieldLabel l ->
                error t.loc ("duplicate field `" ^ l ^ "`"))
                (*    ^^^^^ TODO: Use field location *)
    | _ ->
        error t.loc "expression expected"

  let rec elab_format (context : LocalContext.t) (t : tm) : R.is_format =
    match t.data with
    | Empty ->
        R.Format.pure R.Unit.intro
    | Name name -> begin
        match LocalContext.lookup name context with
        | Some (`Item var) ->
            R.Format.item var
            |> R.handle_local (function
                | `FormatExpected -> error t.loc "format expected"
                | `UnboundVariable -> bug t.loc "unbound item variable")
        | Some (`FormatUniv | `TypeUniv | `Type _ | `Local _) ->  error t.loc "format expected"
        | None -> error t.loc ("unbound variable `" ^ name ^ "`")
    end
    | Int i ->
        R.Format.byte (byte_set_of_int t.loc i)
    | Range (start, stop) ->
        R.Format.byte (byte_set_of_range start stop)
    | Not t -> begin
        match t.data with
        | Int i ->
            R.Format.byte (byte_set_of_int t.loc i |> Byte_set.neg)
        | Range (start, stop) ->
            R.Format.byte (byte_set_of_range start stop |> Byte_set.neg)
        | _ ->
            error t.loc "Can only apply `!_` to bytes and byte ranges"
    end
    | Seq (t0, t1) ->
        R.Format.seq (elab_format context t0) (elab_format context t1)
        |> R.handle_local (function
            | `AmbiguousFormat -> error t.loc "ambiguous concatenation")
    | Union (t0, t1) ->
        R.Format.union (elab_format context t0) (elab_format context t1)
        |> R.handle_local (function
            | `AmbiguousFormat -> error t.loc "ambiguous alternation"
            | `ReprMismatch (_, _) -> error t.loc "mismatched represenations")
    | Action (f, (name, e)) ->
        R.Format.map
          (name, fun x -> elab_synth_ty (context |> LocalContext.bind_local (name, x)) e)
          (elab_format context f)
    | Proj (_, _) -> error t.loc "format expected"
    | RecordEmpty ->
        R.Format.pure R.Record.intro_empty
    | RecordFormat fields ->
        let rec go context lit_fields =
          function
          | [] ->
              R.Record.intro (List.rev lit_fields)
              |> R.handle_local (function
                  | `DuplicateFieldLabel l ->
                      error t.loc ("duplicate field `" ^ l ^ "`"))
                      (*    ^^^^^ TODO: Use field location *)
              |> R.Format.pure
          | (l, f) :: fields ->
              R.Format.flat_map
                (l, fun x ->
                  let var =
                    R.Structural.local x
                    |> R.handle_local (function
                        | `UnboundVariable -> bug t.loc "unbound local variable")
                        (*                        ^^^^^ TODO: Use field location *)
                  in
                  go (context |> LocalContext.bind_local (l, x)) ((l, var) :: lit_fields) fields)
                (elab_format context f)
              |> R.handle_local (function
                  | `AmbiguousFormat -> error t.loc "ambiguous record")
                  (*                          ^^^^^ TODO: Use field location *)
        in
        go context [] fields
    | RecordType _ -> error t.loc "format expected"
    | RecordLit _ -> error t.loc "format expected"

  let rec elab_ty (context : ItemContext.t) (t : tm) : R.is_ty =
    match t.data with
    | Empty ->
        R.Unit.form
    | Name name -> begin
        match ItemContext.lookup name context with
        | Some (`Item var) ->
            R.Structural.item_ty var
            |> R.handle_item (function
                | `TypeExpected -> error t.loc "type expected"
                | `UnboundVariable -> bug t.loc "unbound item variable")
        | Some (`Type t) -> t
        | Some (`FormatUniv | `TypeUniv) -> error t.loc "type expected"
        | None -> error t.loc ("unbound variable `" ^ name ^ "`")
    end
    | Seq (t0, t1) ->
        R.Pair.form (elab_ty context t0) (elab_ty context t1)
    | Proj (t, "Repr") ->
        R.Format.repr (elab_format (LocalContext.of_item_context context) t)
    | RecordEmpty ->
        R.Record.form_empty
    | RecordType fields ->
        R.Record.form (fields |> List.map (fun (l, t) -> l, elab_ty context t))
        |> R.handle_item (function
            | `DuplicateFieldLabel l ->
                error t.loc ("duplicate field `" ^ l ^ "`"))
                (*    ^^^^^ TODO: Use field location *)
    | _ ->
        error t.loc "type expected"

  let elab_ann (context : ItemContext.t) (t : tm) : [`FormatUniv | `TypeUniv | `Type of R.is_ty] =
    match t.data with
    | Name name -> begin
        match ItemContext.lookup name context with
        | Some (`FormatUniv | `TypeUniv | `Type _ as i) -> i
        | Some (`Item _) -> error t.loc "invalid annotation" (* FIXME: Type aliases *)
        | None -> error t.loc ("unbound variable `" ^ name ^ "`")
    end
    | Empty
    | Seq (_, _)
    | RecordEmpty
    | RecordType _ ->
        `Type (elab_ty context t)
    | _ ->
        error t.loc "invalid annotation"

  let elab_program (context : ItemContext.t) (p : program) : R.is_program =
    let rec go context =
      function
      | [] ->
          R.Program.empty
      | (name, None, t) :: rest ->
          R.Program.def_format (name, elab_format (LocalContext.of_item_context context) t)
            (fun var -> go (ItemContext.def_item (name, var) context) rest)
            (* FIXME:   ^^ tailcall? *)
      | (name, Some ann, t) :: rest -> begin
          match elab_ann context ann with
          | `FormatUniv ->
              R.Program.def_format (name, elab_format (LocalContext.of_item_context context) t)
                (fun var -> go (ItemContext.def_item (name, var) context) rest)
                (* FIXME:   ^^ tailcall? *)
          | `TypeUniv ->
              R.Program.def_ty (name, elab_ty context t)
                (fun var -> go (ItemContext.def_item (name, var) context) rest)
                (* FIXME:   ^^ tailcall? *)
          | `Type ann ->
              R.Program.def_expr (name, ann, elab_check_ty (LocalContext.of_item_context context) t)
                (fun var -> go (ItemContext.def_item (name, var) context) rest)
                (* FIXME:   ^^ tailcall? *)
      end
    in
    go context p.items

end

let elab_program p =
  Elab.elab_program Elab.ItemContext.empty p
  |> Core.Refiner.run_item
