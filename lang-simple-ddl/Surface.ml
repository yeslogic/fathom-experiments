(** Surface language

    This language closely mirrors what the programmer originaly wrote,
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
type binder =
  string located

(** Unary operators *)
type op1 = [
  | `Neg
  | `LogicalNot
]

(** Binary operators *)
type op2 = [
  | [ `Eq | `Ne | `Le | `Lt | `Ge | `Gt ]
  | `LogicalOr
  | `LogicalXor
  | `LogicalAnd
  | [ `LogicalShl | `ArithShr | `LogicalShr ]
  | [ `Add | `Sub ]
  | [ `Mul | `Div ]
]

type tm =
  tm_node located

and tm_node =
  | Name of string * tm list
  | Ann of tm * tm
  | Let of binder * tm option * tm * tm
  | Bind of binder * tm * tm
  | RecordLit of (string located * tm) list
  | IntLit of string
  | Proj of tm * string located
  | IfThenElse of tm * tm * tm
  | Op1 of op1 * tm
  | Op2 of op2 * tm * tm

type format_field =
  | Let of binder * tm option * tm          (* let x : tm := tm *)
  | Bind of binder * tm                     (* let x <- tm *)
  | LetField of binder * tm option * tm     (* x : tm := tm *)
  | BindField of binder * tm                (* x <- tm *)
  (* TODO: add `where ...` *)

type item =
  | RecordType of binder * (string located * tm) list
  | RecordFormat of binder * format_field list
  | FormatDef of binder * tm
  | TypeDef of binder * tm
  | TermDef of binder * tm option * tm

type program =
  item list


(** Elaboration from the surface language to the core language. *)
module Elab : sig

  exception Error of loc * string

  val check_program : item list -> Core.program

end = struct

  (** {1 Error handling} *)

  exception Error of loc * string

  let error (type a) (loc : loc) (msg : string) : a =
    raise (Error (loc, msg))

  (** {1 Elaboration context} *)

  type context = {
    items : (string * Core.item) list;
    locals : (string * Core.Semantics.vty) list;
  }

  let extend_item (ctx : context) (name : string) (item : Core.item) : context = {
    ctx with
    items = (name, item) :: ctx.items;
  }

  let extend_local (ctx : context) (name : string) (vty : Core.Semantics.vty) : context = {
    ctx with
    locals = (name, vty) :: ctx.locals;
  }

  let lookup_local (ctx : context) (name : string) : (Core.expr * Core.Semantics.vty) option =
    ctx.locals |> List.find_mapi @@ fun index (name', vt) ->
      if name = name' then Some (Core.LocalVar index, vt) else None

  let eval_ty (ctx : context) : Core.ty -> Core.Semantics.vty =
    Core.Semantics.eval_ty ctx.items

  let quote_vty : ?unfold_items:bool -> Core.Semantics.vty -> Core.ty =
    Core.Semantics.quote_vty

  let format_ty (ctx : context) (fmt : Core.format) : Core.ty =
    Core.Semantics.format_ty ctx.items fmt

  (* Compare two types for equality. *)
  let unify_vtys (ctx : context) (loc : loc) (vt1 : Core.Semantics.vty) (vt2 : Core.Semantics.vty) =
    try Core.Semantics.unify_vtys ctx.items vt1 vt2 with
    | Core.Semantics.FailedToUnify ->
      error loc
        (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %a@]@ @[found: %a@]@]"
          Core.pp_print_ty (quote_vty vt1)
          Core.pp_print_ty (quote_vty vt2))


  (** {1 Bidirectional elaboration} *)

  (** An elaborated term *)
  type elab_tm =
    | KindTm of [`Type | `Format]
    | TypeTm of Core.ty
    | ExprTm of Core.expr * Core.Semantics.vty
    | FormatTm of Core.format

  (** Elaborate a surface term into a core type. *)
  let rec check_type (ctx : context) (tm : tm) : Core.ty =
    match tm.data with
    (* Conversion *)
    | _ ->
      match infer ctx tm with
      | TypeTm ty -> ty
      | FormatTm fmt -> Core.Semantics.format_ty ctx.items fmt
      (* | FormatTm fmt -> FormatRepr fmt *)
      | KindTm _ -> error tm.loc "expected type, found kind"
      | ExprTm (_, _) -> error tm.loc "expected type, found expression"

  (** Elaborate a surface term into a core expression, given an expected type. *)
  and check_expr (ctx : context) (tm : tm) (vty : Core.Semantics.vty) : Core.expr =
    match tm.data with
    | Let (name, def_ty, def, body) ->
        let def, def_vty = infer_ann_expr ctx def def_ty in
        let body = check_expr (extend_local ctx name.data def_vty) body vty in
        Let (name.data, quote_vty def_vty, def, body)

    | RecordLit field_tms ->
        begin match Core.Semantics.force_vty vty with
        | RecordType (name, field_tys) ->
            let rec go field_tms field_exprs =
              match field_tms with
              | [] -> field_exprs
              | (label, _) :: _ when Core.LabelMap.mem label.data field_exprs ->
                  error label.loc (Format.asprintf "duplicate field `%s`" label.data)
              | (label, tm) :: field_tms ->
                  begin match Core.LabelMap.find_opt label.data field_tys with
                  | None -> error label.loc (Format.asprintf "unexpected field `%s`" label.data)
                  | Some vty ->
                      let def = check_expr ctx tm vty in
                      (go [@tailcall]) field_tms (Core.LabelMap.add label.data def field_exprs)
                  end
            in
            RecordLit (name, go field_tms Core.LabelMap.empty)
        | _ ->
          error tm.loc
            (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %s@]@ @[found: %a@]@]"
              "record type"
              Core.pp_print_ty (quote_vty vty))
        end

    | IfThenElse (head, tm1, tm2) ->
        let head = check_expr ctx head BoolType in
        let expr1 = check_expr ctx tm1 vty in
        let expr2 = check_expr ctx tm2 vty in
        BoolElim (head, expr1, expr2)

    (* Conversion *)
    | _ ->
        let expr', vty' = infer_expr ctx tm in
        unify_vtys ctx tm.loc vty vty';
        expr'

  (** Elaborate a surface term into a core format. *)
  and check_format (ctx : context) (tm : tm) : Core.format =
    match tm.data with
    | Let (name, def_ty, def, body) ->
        let def, def_vty = infer_ann_expr ctx def def_ty in
        let body = check_format (extend_local ctx name.data def_vty) body in
        Bind (name.data, Pure (quote_vty def_vty, def), body)

    | IfThenElse (head, tm1, tm2) ->
        let head = check_expr ctx head BoolType in
        let fmt1 = check_format ctx tm1 in
        let fmt2 = check_format ctx tm2 in
        let vty1 = eval_ty ctx (format_ty ctx fmt1) in
        let vty2 = eval_ty ctx (format_ty ctx fmt2) in
        unify_vtys ctx tm.loc vty1 vty2;
        BoolElim (head, fmt1, fmt2)

    (* Conversion *)
    |_ ->
      match infer ctx tm with
      | FormatTm fmt -> fmt
      | KindTm _ -> error tm.loc "expected format, found kind"
      | TypeTm _ -> error tm.loc "expected format, found type"
      | ExprTm (expr, vty) -> Pure (quote_vty vty, expr)

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer (ctx : context) (tm : tm) : elab_tm =
    match tm.data with
    | Name (name, args) ->
        (* TODO: Clean this up and make it less error-prone! *)
        begin match lookup_local ctx name with
        | Some (e, vt) ->
            begin match args with
            | [] -> ExprTm (e, vt)
            | _ -> error tm.loc (Format.asprintf "arity mismatch for `%s`" name)
            end
        | None ->
            begin match List.assoc_opt name ctx.items with
            | Some elab_tm ->
                begin match elab_tm, args with
                | RecordType _, [] -> TypeTm (ItemVar name)
                | TypeDef _, [] -> TypeTm (ItemVar name)
                | FormatDef _, [] -> FormatTm (ItemVar name)
                | ExprDef (ty, _), [] -> ExprTm (ItemVar name, eval_ty ctx ty)
                | _, _ -> error tm.loc (Format.asprintf "arity mismatch for `%s`" name)
                end
            | None ->
                begin match name, args with
                | "Type", [] -> KindTm `Type
                | "Format", [] -> KindTm `Format
                | "List", [ty] ->
                    let ty = check_type ctx ty in
                    TypeTm (ListType ty)
                | "Int", [] -> TypeTm IntType
                | "repeat-len", [len; fmt] ->
                    let len = check_expr ctx len IntType in
                    let fmt = check_format ctx fmt in
                    FormatTm (RepeatLen (len, fmt))
                | "pure", [ty; expr] ->
                    let ty = check_type ctx ty in
                    let expr = check_expr ctx expr (eval_ty ctx ty) in
                    FormatTm (Pure (ty, expr))
                | "byte", [] -> FormatTm Byte
                | "fail", [ty] ->
                    let ty = check_type ctx ty in
                    FormatTm (Fail ty)
                | ("Type" | "Format" | "List" | "Int" | "repeat-len" | "pure" | "byte" | "fail"), _ ->
                    error tm.loc (Format.asprintf "arity mismatch for `%s`" name)
                | _, _ -> error tm.loc (Format.asprintf "unbound name `%s`" name)
                end
            end
        end

    | Ann (tm, ann) ->
        infer_ann ctx tm (Some ann)

    | Let (name, def_ty, def, body) ->
        let def, def_vty = infer_ann_expr ctx def def_ty in
        begin match infer (extend_local ctx name.data def_vty) body with
        | ExprTm (body, vty) -> ExprTm (Let (name.data, quote_vty def_vty, def, body), vty)
        | FormatTm body_fmt -> FormatTm (Bind (name.data, Pure (quote_vty def_vty, def), body_fmt))
        | KindTm _ -> error tm.loc "expected expression or format, found kind"
        | TypeTm _ -> error tm.loc "expected expression or format, found type"
        end

    | Bind (name, def_fmt, body_fmt) ->
        let def_fmt = check_format ctx def_fmt in
        let def_vty = eval_ty ctx (format_ty ctx def_fmt) in
        let body_fmt = check_format (extend_local ctx name.data def_vty) body_fmt in
        FormatTm (Bind (name.data, def_fmt, body_fmt))

    | RecordLit _ ->
        (* TODO: postpone elaboration *)
        error tm.loc "ambiguous record literal"

    | IntLit s ->
        (* TODO: postpone elaboration *)
        ExprTm (IntLit (int_of_string s), IntType)

    | Proj (head, label) ->
        begin match infer ctx head with
        | ExprTm (head, head_vty) ->
            begin match Core.Semantics.force_vty head_vty with
            | RecordType (_, field_vtys) ->
                begin match Core.LabelMap.find_opt label.data field_vtys with
                | Some vty -> ExprTm (RecordProj (head, label.data), vty)
                | None -> error label.loc (Format.sprintf "unknown field `%s`" label.data)
                end
            | _ -> error label.loc (Format.sprintf "unknown field `%s`" label.data)
            end
        | FormatTm fmt when label.data = "Repr" -> TypeTm (format_ty ctx fmt)
        (* | FormatTm fmt when label.data = "Repr" -> TypeTm (FormatRepr fmt) *)
        | _ -> error label.loc (Format.sprintf "unknown field `%s`" label.data)
        end

    | IfThenElse (head, tm1, tm2) ->
        let head = check_expr ctx head BoolType in

        begin match infer ctx tm1, infer ctx tm2 with
        | ExprTm (expr1, vty1), ExprTm (expr2, vty2) ->
            unify_vtys ctx tm.loc vty1 vty2;
            ExprTm (BoolElim (head, expr1, expr2), vty1)
        | FormatTm fmt1, FormatTm fmt2 ->
            let vty1 = eval_ty ctx (format_ty ctx fmt1) in
            let vty2 = eval_ty ctx (format_ty ctx fmt2) in
            unify_vtys ctx tm.loc vty1 vty2;
            FormatTm (BoolElim (head, fmt1, fmt2))
        | _, _ ->
            error tm.loc (Format.sprintf "mismatched arms of if expression")
        end

    | Op1 (op, tm) ->
        let op : Core.prim =
          match op with
          | `Neg -> IntNeg
          | `LogicalNot -> IntLogicalNot
        in
        let expr = check_expr ctx tm IntType in
        ExprTm (PrimApp (op, [expr]), IntType)

    | Op2 (op, tm1, tm2) ->
        let (op : Core.prim), (vty : Core.Semantics.vty) =
          match op with
          | `Eq -> IntEq, BoolType
          | `Ne -> IntNe, BoolType
          | `Le -> IntLe, BoolType
          | `Lt -> IntLt, BoolType
          | `Gt -> IntGt, BoolType
          | `Ge -> IntGe, BoolType
          | `Add -> IntAdd, IntType
          | `Sub -> IntSub, IntType
          | `Mul -> IntMul, IntType
          | `Div -> IntDiv, IntType
          | `LogicalAnd -> IntLogicalAnd, IntType
          | `LogicalOr -> IntLogicalOr, IntType
          | `LogicalXor -> IntLogicalXor, IntType
          | `LogicalShl -> IntLogicalShl, IntType
          | `ArithShr -> IntArithShr, IntType
          | `LogicalShr -> IntLogicalShr, IntType
        in
        let expr1 = check_expr ctx tm1 IntType in
        let expr2 = check_expr ctx tm2 IntType in
        ExprTm (PrimApp (op, [expr1; expr2]), vty)

  and infer_ann (ctx : context) (tm : tm) (ann : tm option) : elab_tm =
    match ann with
    | None -> infer ctx tm
    | Some ann ->
        begin match infer ctx ann with
        | KindTm `Type -> TypeTm (check_type ctx tm)
        | KindTm `Format -> FormatTm (check_format ctx tm)
        | TypeTm ty ->
            let vty = eval_ty ctx ty in
            ExprTm (check_expr ctx tm vty, vty)
        | ExprTm _ ->
            error ann.loc "expected annotation, found expression"
        | FormatTm fmt ->
            let repr = eval_ty ctx (format_ty ctx fmt) in
            (* TODO: Format projection in core language *)
            (* let repr = eval_ty ctx (FormatRepr fmt) in *)
            ExprTm (check_expr ctx tm repr, repr)
        end

  (** Elaborate a surface term into a core expression, inferring its type. *)
  and infer_expr (ctx : context) (tm : tm) : Core.expr * Core.Semantics.vty =
    match infer ctx tm with
    | ExprTm (expr, vty) -> expr, vty
    | KindTm _ -> error tm.loc "expected expression, found kind"
    | TypeTm _ -> error tm.loc "expected expression, found type"
    | FormatTm _ -> error tm.loc "expected expression, found format"

  and infer_ann_expr (ctx : context) (tm : tm) (ty : tm option) : Core.expr * Core.Semantics.vty =
    match ty with
    | Some ty ->
        let ty = check_type ctx ty in
        let vty = eval_ty ctx ty in
        check_expr ctx tm vty, vty
    | None -> infer_expr ctx tm

  (** {1 Top-level elaboration} *)

  (** Elaborate and item and add it to the elaboration context *)
  let check_item (ctx : context) (item : item) : context =
    match item with
    | RecordType (name, field_tms) ->
        let rec go field_tms decls =
          match field_tms with
          | [] -> decls
          | (label, tm) :: field_tms  ->
            if Core.LabelMap.mem label.data decls then
              error label.loc (Format.asprintf "duplicate field labels `%s`" label.data)
            else
              (go [@tailcall]) field_tms (Core.LabelMap.add label.data (check_type ctx tm) decls)
        in
        extend_item ctx name.data (RecordType (go field_tms Core.LabelMap.empty))

    | RecordFormat (name, fmt_fields) ->
        (* TODO: Figure out a better solution to this! *)
        let record_name = name.data ^ "-record" in

        (* Elaborate format fields into a list of field declarations (to be used
           in the record type declaration) and a format for the record type. *)
        let rec go ctx fmt_fields decls : Core.ty Core.LabelMap.t * Core.format =
          match fmt_fields with
          | [] ->
              let defns =
                decls |> Core.LabelMap.mapi @@ fun label _ ->
                  lookup_local ctx label |> Option.get |> fst
              in
              decls, Pure (ItemVar record_name, RecordLit (record_name, defns))

          | fmt_field :: fmt_fields ->
              let label =
                match fmt_field with
                | Let (label, _, _)
                | Bind (label, _)
                | LetField (label, _, _)
                | BindField (label, _) -> label
              in

              if Core.LabelMap.mem label.data decls then
                error label.loc (Format.asprintf "duplicate field labels `%s`" label.data);

              begin match fmt_field with
              (* let x : def_ty := def; fmt_fields... *)
              | Let (_, def_ty, def) ->
                  let def, def_vty = infer_ann_expr ctx def def_ty in
                  let decls, body_fmt = go (extend_local ctx label.data def_vty) fmt_fields decls in
                  decls, Bind (label.data, Pure (quote_vty def_vty, def), body_fmt)

              (* let x <- fmt; fmt_fields... *)
              | Bind (_, fmt) ->
                  let fmt = check_format ctx fmt in
                  let fmt_vty = eval_ty ctx (format_ty ctx fmt) in
                  let decls, body_fmt = go (extend_local ctx label.data fmt_vty) fmt_fields decls in
                  decls, Bind (label.data, fmt, body_fmt)

              (* x : def_ty := def; fmt_fields... *)
              | LetField (_, def_ty, def) ->
                  let def, def_vty = infer_ann_expr ctx def def_ty in
                  (* Add a new field to the record type *)
                  let decls = Core.LabelMap.add label.data (quote_vty def_vty) decls in
                  let decls, body_fmt = go (extend_local ctx label.data def_vty) fmt_fields decls in
                  decls, Bind (label.data, Pure (quote_vty def_vty, def), body_fmt)

              (* x <- fmt; fmt_fields... *)
              | BindField (_, fmt) ->
                  let fmt = check_format ctx fmt in
                  let fmt_vty = eval_ty ctx (format_ty ctx fmt) in
                  (* Add a new field to the record type *)
                  let decls = Core.LabelMap.add label.data (quote_vty fmt_vty) decls in
                  let decls, body_fmt = go (extend_local ctx label.data fmt_vty) fmt_fields decls in
                  decls, Bind (label.data, fmt, body_fmt)
                end
        in

        let decls, fmt = go ctx fmt_fields Core.LabelMap.empty in
        let ctx = extend_item ctx record_name (RecordType decls) in
        extend_item ctx name.data (FormatDef fmt)

    | FormatDef (name, tm) ->
        extend_item ctx name.data (FormatDef (check_format ctx tm))

    | TypeDef (name, tm) ->
        extend_item ctx name.data (TypeDef (check_type ctx tm))

    | TermDef (name, ann, tm) ->
        begin match infer_ann ctx tm ann with
        | KindTm _ -> error name.loc "kind definitions are not supported"
        | TypeTm ty -> extend_item ctx name.data (TypeDef ty)
        | ExprTm (expr, ty) -> extend_item ctx name.data (ExprDef (quote_vty ty, expr))
        | FormatTm fmt -> extend_item ctx name.data (FormatDef fmt)
        end

  let check_program (items : item list) : Core.program =
    let module StringMap = Map.Make (String) in
    let module StringSet = Set.Make (String) in

    let item_name (item : item) : string located =
      match item with
      | RecordType (name, _) -> name
      | RecordFormat (name, _) -> name
      | FormatDef (name, _) -> name
      | TypeDef (name, _) -> name
      | TermDef (name, _, _) -> name
    in

    let item_name_ids =
      Seq.fold_lefti
        (fun map i item ->
          let name = item_name item in
          if StringMap.mem name.data map then
            error name.loc (Format.sprintf "the item name `%s` is defined multiple times" name.data)
          else
            StringMap.add name.data i map)
        StringMap.empty
        (List.to_seq items)
    in

    let rec tm_deps (locals : StringSet.t)  (tm : tm) : int list =
      match tm.data with
      | Name (name, args) ->
          let name =
            if StringSet.mem name locals then [] else
              StringMap.find_opt name item_name_ids |> Option.to_list
          in
          name @ List.concat_map (tm_deps locals) args
      | Ann (tm, ann) ->
          tm_deps locals tm
            @ tm_deps locals ann
      | Let (name, None, def_tm, body_tm) ->
          tm_deps locals def_tm
            @ tm_deps (StringSet.add name.data locals) body_tm
      | Let (name, Some def_ty, def_tm, body_tm) ->
          tm_deps locals def_ty
            @ tm_deps locals def_tm
            @ tm_deps (StringSet.add name.data locals) body_tm
      | Bind (name, def_fmt, body_fmt) ->
          tm_deps locals def_fmt
            @ tm_deps (StringSet.add name.data locals) body_fmt
      | RecordLit field_tms ->
          List.concat_map (fun (_, tm) -> tm_deps locals tm) field_tms
      | IntLit _ -> []
      | Proj (tm, _) -> tm_deps locals tm
      | IfThenElse (head, tm1, tm2) ->
          tm_deps locals head
            @ tm_deps locals tm1
            @ tm_deps locals tm2
      | Op1 (_, tm) -> tm_deps locals tm
      | Op2 (_, tm1, tm2) -> tm_deps locals tm1 @ tm_deps locals tm2
    in

    (* Collect a dependency list for use when topologically sorting *)
    let collect_deps (items : item list) : (int * int list) list =
      items |> List.mapi @@ fun id item ->
        match item with
        | RecordType (_, field_tms) ->
            id, List.concat_map (fun (_, tm) -> tm_deps StringSet.empty tm) field_tms
        | RecordFormat (_, field_tms) ->
            let rec go locals (field_tms : format_field list) =
              match field_tms with
              | [] -> []
              | Let (label, None, tm) :: field_tms
              | Bind (label, tm) :: field_tms
              | LetField (label, None, tm) :: field_tms
              | BindField (label, tm) :: field_tms ->
                  tm_deps locals tm
                    @ go (StringSet.add label.data locals) field_tms
              | Let (label,Some ann, tm) :: field_tms
              | LetField (label,Some ann, tm) :: field_tms ->
                  tm_deps locals ann
                    @ tm_deps locals tm
                    @ go (StringSet.add label.data locals) field_tms
            in
            id, go StringSet.empty field_tms
        | FormatDef (_, tm) -> id, tm_deps StringSet.empty tm
        | TypeDef (_, tm) -> id, tm_deps StringSet.empty tm
        | TermDef (_, None, tm) -> id, tm_deps StringSet.empty tm
        | TermDef (_, Some ann, tm) -> id, tm_deps StringSet.empty ann @ tm_deps StringSet.empty tm
    in

    let rec go (ctx : context) (order : int list) =
      match order with
      | [] -> List.rev ctx.items
      | id :: order -> (go [@tailcall]) (check_item ctx (List.nth items id)) order
    in

    (* TODO: Sort with strongly connected components, elaborating to fixed-points *)
    match Tsort.sort (collect_deps items) with
    | Tsort.Sorted order -> go { items = []; locals = [] } order
    | Tsort.ErrorCycle _ -> failwith "TODO: cyclic items" (* TODO: raise a better error *)

end
