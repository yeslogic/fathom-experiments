(** {0 Surface language}

    The surface language closely mirrors what the programmer originaly wrote,
    including syntactic sugar and higher level language features that make
    programming more convenient (in comparison to the {!Core}).
*)

(** {1 Syntax} *)

(** The start and end position in a source file *)
type loc =
  Lexing.position * Lexing.position

(** Located nodes *)
type 'a located = {
  loc : loc;
  data : 'a;
}

(** Names that bind definitions or parameters *)
type binder = string located

type tm =
  tm_node located

and tm_node =
  | Name of string
  | Ann of tm * tm
  | RecordEmpty
  | RecordTy of (string located * tm) list
  | RecordLit of (string located * tm) list
  | RecordFormat of (string located * tm) list
  | ActionFormat of tm * (binder * tm)
  | RangeFormat of bound * bound
  | Tuple of tm list
  | Int of int
  | Proj of tm * [`Label of string located | `Index of int located]
  | Op1 of [`Not] * tm
  | Op2 of [`Or] * tm * tm

and bound =
  | Open
  | Inclusive of int located
  | Exclusive of int located

type item =
  | FormatDef of binder * tm
  | TypeDef of binder * tm
  | Def of binder * tm option * tm

type program =
  item list


(** {1 Elaboration} *)

module Elab : sig

  exception Error of loc * string
  exception Bug of loc * string

  val check_program : item list -> Core.program

end = struct

  module LabelMap = Core.LabelMap


  (** {2 Error handling} *)

  exception Error of loc * string
  exception Bug of loc * string

  let error loc msg = raise (Error (loc, msg))
  let[@warning "-unused-value-declaration"] bug loc msg = raise (Bug (loc, msg))


  (** {2 Byte conversions} *)

  let byte_of_int loc i =
    if 0 <= i && i <= 255 then
      Char.chr i
    else
      error loc (Format.asprintf "integer `%i` is outside the range `0..255`" i)

  let byte_set_of_int loc i =
    ByteSet.singleton (byte_of_int loc i)

  let byte_set_of_range (start : bound) (stop : bound) =
    let start =
      match start with
      | Open -> Char.chr 0
      | Inclusive start -> byte_of_int start.loc start.data
      | Exclusive start -> byte_of_int start.loc (start.data + 1)
    and stop =
      match stop with
      | Open -> Char.chr 255
      | Inclusive stop -> byte_of_int stop.loc stop.data
      | Exclusive stop -> byte_of_int stop.loc (stop.data - 1)
    in
    ByteSet.range start stop

  let format_of_byte_set s : Core.format = {
    node = Byte s;
    info = Core.FormatInfo.byte s;
  }


  (** {2 Elaboration contexts} *)

  type context = {
    items : (string * Core.item) list;
    locals : (string * Core.Semantics.vty) list;
  }

  let lookup_local (ctx : context) (n : string) : (Core.expr * Core.Semantics.vty) option =
    ctx.locals |> List.find_mapi @@ fun i (n', t) ->
      if n = n' then Some (Core.Local i, t) else None

  let eval_ty (ctx : context) : Core.ty -> Core.Semantics.vty =
    Core.Semantics.eval_ty ctx.items

  let quote_ty : Core.Semantics.vty -> Core.ty =
    Core.Semantics.quote_ty


  (** {2 Bidirectional type checking} *)

  (** An elaborated term *)
  type elab_tm =
    | KindTm of [`Type | `Format]
    | TypeTm of Core.ty
    | ExprTm of Core.expr * Core.Semantics.vty
    | FormatTm of Core.format

  (* Compare two types for equality. *)
  let unify_tys (loc : loc) (vt1 : Core.Semantics.vty) (vt2 : Core.Semantics.vty) =
    if Core.Semantics.unify_tys vt1 vt2 then () else
      error loc
        (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %a@]@ @[found: %a@]@]"
          Core.pp_print_ty (quote_ty vt1)
          Core.pp_print_ty (quote_ty vt2))

  (** Elaborate a surface term into a core type. *)
  let rec check_type (ctx : context) (tm : tm) : Core.ty =
    match tm.data with
    (* Empty records *)
    | RecordEmpty ->
      RecordTy LabelMap.empty

    (* Tuples *)
    | Tuple tms ->
      TupleTy (List.map (check_type ctx) tms)

    (* Integer literals *)
    | Int _ ->
      error tm.loc "unexpected integer literal"

    (* Conversion *)
    | _ ->
      match infer ctx tm with
      | KindTm _ -> error tm.loc "expected type, found kind"
      | TypeTm t -> t
      | ExprTm (_, _) -> error tm.loc "expected type, found expression"
      | FormatTm f -> FormatRepr f

  (** Elaborate a surface term into a core expression, given an expected type. *)
  and check_expr (ctx : context) (tm : tm) (vt : Core.Semantics.vty) : Core.expr =
    match tm.data, vt with
    (* Empty records *)

    | RecordEmpty, vt ->
      unify_tys tm.loc vt (RecordTy LabelMap.empty);
      RecordLit LabelMap.empty

    (* Tuples *)

    | Tuple tms, TupleTy vts ->
      let rec go tms vts =
        match tms, vts with
        | [], [] -> []
        | tm :: tms, vt :: vts -> check_expr ctx tm vt :: go tms vts
        | _, _ -> error tm.loc "unexpected number of elements in tuple"
      in
      TupleLit (go tms vts)

    | Tuple _, vt ->
      error tm.loc
        (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %a@]@ @[found: tuple@]@]"
          Core.pp_print_ty (quote_ty vt))

    (* Integer literals *)

    | Int i, ByteTy -> ByteLit (byte_of_int tm.loc i)
    | Int _, _ -> error tm.loc "unexpected integer literal"

    (* Conversion *)

    | _, vt ->
      let e', vt' = infer_expr ctx tm in
      unify_tys tm.loc vt vt';
      e'

  (** Elaborate a surface term into a core format. *)
  and check_format (ctx : context) (tm : tm) : Core.format =
    match tm.data with
    (* Empty records *)
    | RecordEmpty -> {
      node = Pure (RecordTy LabelMap.empty, RecordLit LabelMap.empty);
      info = Core.FormatInfo.empty;
    }

    (* Tuples *)
    | Tuple tms ->
      let fs = List.map (check_format ctx) tms in
      Core.{
        node = Seq fs;
        info = List.fold_right (fun f acc -> FormatInfo.seq f.info acc) fs FormatInfo.empty;
      }

    | Int i ->
      format_of_byte_set (byte_set_of_int tm.loc i)

    | _ ->
      match infer ctx tm with
      | KindTm _ -> error tm.loc "expected format, found kind"
      | TypeTm _ -> error tm.loc "expected format, found type"
      | ExprTm (_, _) -> error tm.loc "expected format, found expression"
      | FormatTm f -> f

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer (ctx : context) (tm : tm) : elab_tm =
    match tm.data with
    | Name n -> begin
      match lookup_local ctx n with
      | Some (e, vt) -> ExprTm (e, vt)
      | None -> begin
        match List.assoc_opt n ctx.items with
        | Some (Type _) -> TypeTm (Item n)
        | Some (Format f) -> FormatTm { f with node = Item n }
        | Some (Expr (_, t)) -> ExprTm (Item n, eval_ty ctx t)
        | None when n = "Type" -> KindTm `Type
        | None when n = "Format" -> KindTm `Format
        | None when n = "U8" -> TypeTm ByteTy
        | None -> error tm.loc (Format.asprintf "unbound name `%s`" n)
      end
    end

    | Ann (tm, ann) -> begin
      match infer ctx ann with
      | KindTm `Type -> TypeTm (check_type ctx tm)
      | KindTm `Format -> FormatTm (check_format ctx tm)
      | TypeTm t -> ExprTm (check_expr ctx tm (eval_ty ctx t), eval_ty ctx t)
      | ExprTm _ -> error tm.loc "expected annotation, found expression"
      | FormatTm f ->
        let repr = eval_ty ctx (FormatRepr f) in
        ExprTm (check_expr ctx tm repr, repr)
    end

    | RecordEmpty ->
      error tm.loc "ambiguous empty record"

    | RecordTy fs ->
      let rec go fs fs' =
        match fs with
        | [] -> TypeTm (RecordTy fs')
        | (l, tm) :: fs  ->
          if LabelMap.mem l.data fs' then
            error l.loc (Format.asprintf "duplicate field labels `%s`" l.data)
          else
            (go [@tailcall]) fs (LabelMap.add l.data (check_type ctx tm) fs')
      in
      go fs LabelMap.empty

    | RecordLit fs ->
      let rec go fs fs_e fs_vt =
        match fs with
        | [] -> ExprTm (RecordLit fs_e, RecordTy fs_vt)
        | (l, e) :: fs ->
          if LabelMap.mem l.data fs_e then
            error l.loc (Format.asprintf "duplicate field labels `%s`" l.data)
          else
            let e, t = infer_expr ctx e in
            (go [@tailcall]) fs (LabelMap.add l.data e fs_e) (LabelMap.add l.data t fs_vt)
      in
      go fs LabelMap.empty LabelMap.empty

    | RecordFormat fs ->
      let rec go ctx fs_t fs : Core.format =
        match fs with
        | [] ->
          let t = Core.RecordTy fs_t in
          let fs_e = Seq.init (LabelMap.cardinal fs_t) (fun i -> fst (List.nth ctx.locals i), Core.Local i) in
          {
            node = Pure (t, Core.RecordLit (LabelMap.of_seq fs_e));
            info = Core.FormatInfo.empty;
          }
        | (l, f) :: fs ->
          if LabelMap.mem l.data fs_t then
            error l.loc (Format.sprintf "duplicate label in record format `%s`" l.data)
          else
            let f1 = check_format ctx f in
            let fs_t = LabelMap.add l.data (Core.FormatRepr f1) fs_t in
            let f2 = go { ctx with locals = (l.data, eval_ty ctx (FormatRepr f1)) :: ctx.locals } fs_t fs in
            {
              node = FlatMap (FormatRepr f1, (l.data, f2), f1);
              info = Core.FormatInfo.seq f1.info f2.info;
            }
      in
      FormatTm (go ctx LabelMap.empty fs)

    | ActionFormat (f, (n, e)) ->
      let f = check_format ctx f in
      let e, vt = infer_expr { ctx with locals = (n.data, eval_ty ctx (FormatRepr f)) :: ctx.locals } e in
      FormatTm {
        node = Map (quote_ty vt, (n.data, e), f);
        info = f.info;
      }

    | RangeFormat (start, stop) ->
      FormatTm (format_of_byte_set (byte_set_of_range start stop))

    | Int i ->
      (* TODO: postpone elaboration *)
      ExprTm (ByteLit (byte_of_int tm.loc i), ByteTy)

    | Tuple tms ->
      (* TODO: postpone elaboration *)
      let es_ts = List.map (infer_expr ctx) tms in
      ExprTm (TupleLit (List.map fst es_ts), TupleTy (List.map snd es_ts))

    | Proj (e, `Label l) -> begin
      match infer ctx e with
      | ExprTm (e, RecordTy ts) -> begin
        match LabelMap.find_opt l.data ts with
        | Some t -> ExprTm (RecordProj (e, l.data), t)
        | None -> error l.loc (Format.sprintf "unknown field `%s`" l.data)
      end
      | FormatTm f when l.data = "Repr" -> TypeTm (FormatRepr f)
      | _ -> error l.loc (Format.sprintf "unknown field `%s`" l.data)
    end

    | Proj (e, `Index i) -> begin
      match infer ctx e with
      | ExprTm (e, TupleTy ts) when i.data < List.length ts ->
        ExprTm (TupleProj (e, i.data), List.nth ts i.data)
      | _ -> error i.loc (Format.sprintf "unknown field `%i`" i.data)
    end

    | Op1 (`Not, tm) ->
        let s =
          match tm.data with
          | Int i -> byte_set_of_int tm.loc i
          | RangeFormat (start, stop) -> byte_set_of_range start stop
          | _ -> error tm.loc "negation is only supported for integer and range formats"
        in
        FormatTm (format_of_byte_set (ByteSet.neg s))

    | Op2 (`Or, f1, f2) ->
      let f1 = check_format ctx f1 in
      let f2 = check_format ctx f2 in
      unify_tys tm.loc (eval_ty ctx (FormatRepr f1)) (eval_ty ctx (FormatRepr f1));
      FormatTm {
        node = Union (f1, f2);
        info = Core.FormatInfo.union f1.info f2.info;
      }

  and infer_expr (ctx : context) (tm : tm) : Core.expr * Core.Semantics.vty =
    match infer ctx tm with
    | KindTm _ -> error tm.loc "expected expression, found kind"
    | ExprTm (e, vt) -> e, vt
    | TypeTm _ -> error tm.loc "expected expression, found type"
    | FormatTm _ -> error tm.loc "expected expression, found format"


  (** {2 Item traversal} *)

  (** Collect a dependency list for use when topologically sorting *)
  let collect_deps (items : item list) : (int * int list) list =
    let module StringMap = Map.Make (String) in
    let module StringSet = Set.Make (String) in

    let name_map =
      Seq.fold_lefti
        (fun map i item ->
          let n =
            match item with
            | FormatDef (n, _) -> n
            | TypeDef (n, _) -> n
            | Def (n, _, _) -> n
          in
          if StringMap.mem n.data map then
            error n.loc (Format.sprintf "the item name `%s` is defined multiple times" n.data)
          else
            StringMap.add n.data i map)
        StringMap.empty
        (List.to_seq items)
    in

    let rec tm_deps (locals : StringSet.t)  (t : tm) : int list =
      match t.data with
      | Name n when StringSet.mem n locals -> []
      | Name n -> StringMap.find_opt n name_map |> Option.to_list
      | Ann (tm, ann) -> tm_deps locals tm @ tm_deps locals ann
      | RecordEmpty -> []
      | RecordTy fs -> List.concat_map (fun (_, t) -> tm_deps locals t) fs
      | RecordLit fs -> List.concat_map (fun (_, e) -> tm_deps locals e) fs
      | RecordFormat fs ->
        let rec go locals fs =
          match fs with
          | [] -> []
          | (l, f) :: fs ->
            tm_deps locals f @ go (StringSet.add l.data locals) fs
        in
        go locals fs
      | ActionFormat (f, (n, e)) -> tm_deps locals f @ tm_deps (StringSet.add n.data locals) e
      | RangeFormat (_, _) -> []
      | Int _ -> []
      | Tuple ts -> List.concat_map (tm_deps locals) ts
      | Proj (e, _) -> tm_deps locals e
      | Op1 (_, f) -> tm_deps locals f
      | Op2 (_, f1, f2) -> tm_deps locals f1 @ tm_deps locals f2
    in

    items |> List.mapi @@ fun i item ->
      match item with
      | FormatDef (_, f) -> i, tm_deps StringSet.empty f
      | TypeDef (_, t) -> i, tm_deps StringSet.empty t
      | Def (_, None, e) -> i, tm_deps StringSet.empty e
      | Def (_, Some t, e) -> i, tm_deps StringSet.empty t @ tm_deps StringSet.empty e

  let check_program (is : item list) : Core.program =
    let check_item (ctx : context) (i : item) : string * Core.item =
      match i with
      | FormatDef (n, f) -> n.data, Format (check_format ctx f)
      | TypeDef (n, t) -> n.data, Type (check_type ctx t)
      | Def (n, None, body) -> begin
        match infer ctx body with
        | KindTm _ -> error n.loc "kind definitions are not supported"
        | TypeTm t -> n.data, Type t
        | ExprTm (e, vt) -> n.data, Expr (e, quote_ty vt)
        | FormatTm f ->  n.data, Format f
      end
      | Def (n, Some ann, body) -> begin
        match infer ctx ann with
        | KindTm `Type -> n.data, Type (check_type ctx body)
        | KindTm `Format -> n.data, Format (check_format ctx body)
        | TypeTm t -> n.data, Expr (check_expr ctx body (eval_ty ctx t), t)
        | ExprTm _ -> error body.loc "expected annotation, found expression"
        | FormatTm f -> n.data, Expr (check_expr ctx body (eval_ty ctx (FormatRepr f)), FormatRepr f)
      end
    in

    let[@tail_mod_cons] rec go ctx order =
      match order with
      | i :: order ->
        let n, i = check_item ctx (List.nth is i) in
        go { ctx with items = (n, i) :: ctx.items } order
      | [] ->
        Core.{ items = List.rev ctx.items }
    in

    (* TODO: Sort with strongly connected components, elaborating to fixedpoints *)
    match Tsort.sort (collect_deps is) with
    | Tsort.Sorted order -> go { items = []; locals = [] } order
    | Tsort.ErrorCycle _ -> failwith "TODO: cyclic items" (* TODO: Better error *)

end
