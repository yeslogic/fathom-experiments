open Syntax

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
    if name = name' then Some (Core.Local_var index, vt) else None

let eval_ty (ctx : context) : Core.ty -> Core.Semantics.vty =
  Core.Semantics.eval_ty ctx.items

let quote_vty : unfold:bool -> Core.Semantics.vty -> Core.ty =
  Core.Semantics.quote_vty

let format_ty (ctx : context) (fmt : Core.format) : Core.ty =
  Core.Semantics.format_ty ctx.items fmt

(* Compare two types for equality. *)
let unify_vtys (ctx : context) (loc : loc) (vt1 : Core.Semantics.vty) (vt2 : Core.Semantics.vty) =
  try Core.Semantics.unify_vtys ctx.items vt1 vt2 with
  | Core.Semantics.Failed_to_unify ->
    error loc
      (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %a@]@ @[found: %a@]@]"
        Core.pp_ty (quote_vty ~unfold:false vt1 |> Core.zonk_ty)
        Core.pp_ty (quote_vty ~unfold:false vt2 |> Core.zonk_ty))


(** {1 Bidirectional elaboration} *)

(** An elaborated term *)
type elab_tm =
  | Kind_tm of [`Type | `Format]
  | Type_tm of Core.ty
  | Expr_tm of Core.expr * Core.Semantics.vty
  | Format_tm of Core.format

(** Elaborate a surface term into a core type. *)
let rec check_type (ctx : context) (tm : tm) : Core.ty =
  match tm.data with
  (* Conversion *)
  | _ ->
      match infer ctx tm with
      | Type_tm ty -> ty
      | Format_tm fmt -> Format_repr fmt
      | Kind_tm _ -> error tm.loc "expected type, found kind"
      | Expr_tm (_, _) -> error tm.loc "expected type, found expression"

(** Elaborate a surface term into a core expression, given an expected type. *)
and check_expr (ctx : context) (tm : tm) (vty : Core.Semantics.vty) : Core.expr =
  match tm.data with
  | Let (name, def_ty, def, body) ->
      let def, def_vty = infer_ann_expr ctx def def_ty in
      let body = check_expr (extend_local ctx name.data def_vty) body vty in
      Let (name.data, quote_vty ~unfold:false def_vty, def, body)

  | Record_lit field_tms ->
      begin match Core.Semantics.force_all_vty vty with
      | Record_type (name, field_tys) ->
          let rec go field_tms field_exprs =
            match field_tms with
            | [] -> field_exprs
            | (label, _) :: _ when Core.Label_map.mem label.data field_exprs ->
                error label.loc (Format.asprintf "duplicate field `%s`" label.data)
            | (label, tm) :: field_tms ->
                begin match Core.Label_map.find_opt label.data field_tys with
                | None -> error label.loc (Format.asprintf "unexpected field `%s`" label.data)
                | Some vty ->
                    let def = check_expr ctx tm vty in
                    (go [@tailcall]) field_tms (Core.Label_map.add label.data def field_exprs)
                end
          in
          Record_lit (name, go field_tms Core.Label_map.empty)
      | _ ->
        error tm.loc
          (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %s@]@ @[found: %a@]@]"
            "record type"
            Core.pp_ty (quote_vty ~unfold:false vty |> Core.zonk_ty))
      end

  | If_then_else (head, tm1, tm2) ->
      let head = check_expr ctx head Bool_type in
      let expr1 = check_expr ctx tm1 vty in
      let expr2 = check_expr ctx tm2 vty in
      Bool_elim (head, expr1, expr2)

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
      Bind (name.data, Pure (quote_vty ~unfold:false def_vty, def), body)

  | If_then_else (head, tm1, tm2) ->
      let head = check_expr ctx head Bool_type in
      let fmt1 = check_format ctx tm1 in
      let fmt2 = check_format ctx tm2 in
      let vty1 = eval_ty ctx (format_ty ctx fmt1) in
      let vty2 = eval_ty ctx (format_ty ctx fmt2) in
      unify_vtys ctx tm.loc vty1 vty2;
      Bool_elim (head, fmt1, fmt2)

  | Op2 (op, tm1, tm2) ->
      let prim : Core.prim =
        match op with
        | `Eq -> Eq `Int64
        | `Ne -> Ne `Int64
        | `Le -> Le `Int64
        | `Lt -> Lt `Int64
        | `Ge -> Ge `Int64
        | `Gt -> Gt `Int64
        | _ -> error tm.loc "unexpected binary operator in format refinement"
      in
      let fmt1 = check_format ctx tm1 in
      let vty = eval_ty ctx (Format_repr fmt1) in
      unify_vtys ctx tm1.loc vty Int64_type;
      (* FIXME: fresh variables *)
      Bind ("x", fmt1,
        let expr2 = check_expr (extend_local ctx "x" vty) tm2 vty in
        Bool_elim (Prim_app (prim, [Local_var 0; expr2]),
          Pure (Int64_type, Local_var 0),
          Fail Int64_type))

  (* Conversion *)
  | _ ->
    match infer ctx tm with
    | Format_tm fmt -> fmt
    | Kind_tm _ -> error tm.loc "expected format, found kind"
    | Type_tm _ -> error tm.loc "expected format, found type"
    | Expr_tm (expr, vty) -> Pure (quote_vty ~unfold:false vty, expr)

(** Elaborate a surface term into a core term, inferring its type. *)
and infer (ctx : context) (tm : tm) : elab_tm =
  match tm.data with
  | Name (name, args) ->
      (* TODO: Clean this up and make it less error-prone! *)
      begin match lookup_local ctx name with
      | Some (e, vt) ->
          begin match args with
          | [] -> Expr_tm (e, vt)
          | _ -> error tm.loc (Format.asprintf "arity mismatch for `%s`" name)
          end
      | None ->
          begin match List.assoc_opt name ctx.items with
          | Some elab_tm ->
              begin match elab_tm, args with
              | Record_type _, [] | Type_def _, [] -> Type_tm (Item_var name)
              | Format_def _, [] -> Format_tm (Item_var name)
              | Expr_def (ty, _), [] -> Expr_tm (Item_var name, eval_ty ctx ty)
              | _, _ -> error tm.loc (Format.asprintf "arity mismatch for `%s`" name)
              end
          | None ->
              begin match name, args with
              | "Type", [] -> Kind_tm `Type
              | "Format", [] -> Kind_tm `Format
              | "List", [Anon ty] ->
                  let ty = check_type ctx ty in
                  Type_tm (List_type ty)
              | "Int64", [] -> Type_tm Int64_type
              | "repeat-len", [Anon len; Anon fmt] ->
                  let len = check_expr ctx len Int64_type in
                  let fmt = check_format ctx fmt in
                  Format_tm (Repeat_len (len, fmt))
              | "pure", [Labelled ({ data = "A"; _ }, ty); Anon expr] ->
                  let ty = check_type ctx ty in
                  let expr = check_expr ctx expr (eval_ty ctx ty) in
                  Format_tm (Pure (ty, expr))
              | "pure", [Anon expr] ->
                  let ty : Core.ty = Meta_var (Core.Semantics.Meta.fresh ()) in
                  let expr = check_expr ctx expr (eval_ty ctx ty) in
                  Format_tm (Pure (ty, expr))
              | "byte", [] -> Format_tm Byte
              | "fail", [Labelled ({ data = "A"; _ }, ty)] ->
                  let ty = check_type ctx ty in
                  Format_tm (Fail ty)
              | "fail", [] ->
                  Format_tm (Fail (Meta_var (Core.Semantics.Meta.fresh ())))
              | ("Type" | "Format" | "List" | "Int64" | "repeat-len" | "pure" | "byte" | "fail"), _ ->
                  error tm.loc (Format.asprintf "arity mismatch for `%s`" name)
              | _, _ -> error tm.loc (Format.asprintf "unbound name `%s`" name)
              end
          end
      end

  | Placeholder ->
      Type_tm (Meta_var (Core.Semantics.Meta.fresh ()))

  | Ann (tm, ann) ->
      infer_ann ctx tm (Some ann)

  | Let (name, def_ty, def, body) ->
      let def, def_vty = infer_ann_expr ctx def def_ty in
      begin match infer (extend_local ctx name.data def_vty) body with
      | Expr_tm (body, vty) -> Expr_tm (Let (name.data, quote_vty ~unfold:false def_vty, def, body), vty)
      | Format_tm body_fmt -> Format_tm (Bind (name.data, Pure (quote_vty ~unfold:false def_vty, def), body_fmt))
      | Kind_tm _ -> error tm.loc "expected expression or format, found kind"
      | Type_tm _ -> error tm.loc "expected expression or format, found type"
      end

  | Bind (name, def_fmt, body_fmt) ->
      let def_fmt = check_format ctx def_fmt in
      let def_vty = eval_ty ctx (format_ty ctx def_fmt) in
      let body_fmt = check_format (extend_local ctx name.data def_vty) body_fmt in
      Format_tm (Bind (name.data, def_fmt, body_fmt))

  | Record_lit _ ->
      (* TODO: postpone elaboration *)
      error tm.loc "ambiguous record literal"

  | Int_lit s ->
      (* TODO: postpone elaboration *)
      begin match Int64.of_string_opt s with
      | Some i -> Expr_tm (Int64_lit i, Int64_type)
      | None -> error tm.loc "failed to parse integer literal"
      end

  | Proj (head, label) ->
      begin match infer ctx head with
      | Expr_tm (head, head_vty) ->
          begin match Core.Semantics.force_all_vty head_vty with
          | Record_type (_, field_vtys) ->
              begin match Core.Label_map.find_opt label.data field_vtys with
              | Some vty -> Expr_tm (Record_proj (head, label.data), vty)
              | None -> error label.loc (Format.sprintf "unknown field `%s`" label.data)
              end
          | _ -> error label.loc (Format.sprintf "unknown field `%s`" label.data)
          end
      | Format_tm fmt when label.data = "Repr" -> Type_tm (Format_repr fmt)
      | _ -> error label.loc (Format.sprintf "unknown field `%s`" label.data)
      end

  | If_then_else (head, tm1, tm2) ->
      let head = check_expr ctx head Bool_type in

      begin match infer ctx tm1, infer ctx tm2 with
      | Expr_tm (expr1, vty1), Expr_tm (expr2, vty2) ->
          unify_vtys ctx tm.loc vty1 vty2;
          Expr_tm (Bool_elim (head, expr1, expr2), vty1)
      | Format_tm fmt1, Format_tm fmt2 ->
          let vty1 = eval_ty ctx (format_ty ctx fmt1) in
          let vty2 = eval_ty ctx (format_ty ctx fmt2) in
          unify_vtys ctx tm.loc vty1 vty2;
          Format_tm (Bool_elim (head, fmt1, fmt2))
      | _, _ ->
          error tm.loc (Format.sprintf "mismatched arms of if expression")
      end

  | Op1 (op, tm) ->
      let op : Core.prim =
        match op with
        | `Neg -> Neg `Int64
        | `Bit_not -> Bit_not `Int64
      in
      let expr = check_expr ctx tm Int64_type in
      Expr_tm (Prim_app (op, [expr]), Int64_type)

  | Op2 (op, tm1, tm2) ->
      let (op : Core.prim), (vty : Core.Semantics.vty) =
        match op with
        | `Eq -> Eq `Int64, Bool_type
        | `Ne -> Ne `Int64, Bool_type
        | `Le -> Le `Int64, Bool_type
        | `Lt -> Lt `Int64, Bool_type
        | `Gt -> Gt `Int64, Bool_type
        | `Ge -> Ge `Int64, Bool_type
        | `Add -> Add `Int64, Int64_type
        | `Sub -> Sub `Int64, Int64_type
        | `Mul -> Mul `Int64, Int64_type
        | `Div -> Div `Int64, Int64_type
        | `Bit_and -> Bit_and `Int64, Int64_type
        | `Bit_or -> Bit_or `Int64, Int64_type
        | `Bit_xor -> Bit_xor `Int64, Int64_type
        | `Bit_shl -> Bit_shl `Int64, Int64_type
        | `Bit_shr -> Bit_shr `Int64, Int64_type
      in
      let expr1 = check_expr ctx tm1 Int64_type in
      let expr2 = check_expr ctx tm2 Int64_type in
      Expr_tm (Prim_app (op, [expr1; expr2]), vty)

and infer_ann (ctx : context) (tm : tm) (ann : tm option) : elab_tm =
  match ann with
  | None -> infer ctx tm
  | Some ann ->
      begin match infer ctx ann with
      | Kind_tm `Type -> Type_tm (check_type ctx tm)
      | Kind_tm `Format -> Format_tm (check_format ctx tm)
      | Type_tm ty ->
          let vty = eval_ty ctx ty in
          Expr_tm (check_expr ctx tm vty, vty)
      | Expr_tm _ ->
          error ann.loc "expected annotation, found expression"
      | Format_tm fmt ->
          let repr = eval_ty ctx (Format_repr fmt) in
          Expr_tm (check_expr ctx tm repr, repr)
      end

(** Elaborate a surface term into a core expression, inferring its type. *)
and infer_expr (ctx : context) (tm : tm) : Core.expr * Core.Semantics.vty =
  match infer ctx tm with
  | Expr_tm (expr, vty) -> expr, vty
  | Kind_tm _ -> error tm.loc "expected expression, found kind"
  | Type_tm _ -> error tm.loc "expected expression, found type"
  | Format_tm _ -> error tm.loc "expected expression, found format"

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
  | Record_type (name, field_tms) ->
      let rec go field_tms decls =
        match field_tms with
        | [] -> decls
        | (label, tm) :: field_tms  ->
          if Core.Label_map.mem label.data decls then
            error label.loc (Format.asprintf "duplicate field labels `%s`" label.data)
          else
            (go [@tailcall]) field_tms (Core.Label_map.add label.data (check_type ctx tm) decls)
      in
      extend_item ctx name.data (Record_type (go field_tms Core.Label_map.empty))

  | Record_format (name, fmt_fields) ->
      (* TODO: Figure out a better solution to this! *)
      (* FIXME: Could shadow other items *)
      let record_name = Case_conv.capitalised_kebab_case name.data in

      (* Elaborate format fields into a list of field declarations (to be used
          in the record type declaration) and a format for the record type. *)
      let rec go ctx fmt_fields decls : Core.ty Core.Label_map.t * Core.format =
        match fmt_fields with
        | [] ->
            let defns =
              decls |> Core.Label_map.mapi @@ fun label _ ->
                lookup_local ctx label |> Option.get |> fst
            in
            decls, Pure (Item_var record_name, Record_lit (record_name, defns))

        | fmt_field :: fmt_fields ->
            let label =
              match fmt_field with
              | Let (label, _, _)
              | Bind (label, _)
              | Let_field (label, _, _)
              | Bind_field (label, _) -> label
            in

            if Core.Label_map.mem label.data decls then
              error label.loc (Format.asprintf "duplicate field labels `%s`" label.data);

            begin match fmt_field with
            (* let x : def_ty := def; fmt_fields... *)
            | Let (_, def_ty, def) ->
                let def, def_vty = infer_ann_expr ctx def def_ty in
                let decls, body_fmt = go (extend_local ctx label.data def_vty) fmt_fields decls in
                decls, Bind (label.data, Pure (quote_vty ~unfold:false def_vty, def), body_fmt)

            (* let x <- fmt; fmt_fields... *)
            | Bind (_, fmt) ->
                let fmt = check_format ctx fmt in
                let fmt_vty = eval_ty ctx (format_ty ctx fmt) in
                let decls, body_fmt = go (extend_local ctx label.data fmt_vty) fmt_fields decls in
                decls, Bind (label.data, fmt, body_fmt)

            (* x : def_ty := def; fmt_fields... *)
            | Let_field (_, def_ty, def) ->
                let def, def_vty = infer_ann_expr ctx def def_ty in
                (* Add a new field to the record type *)
                let decls = Core.Label_map.add label.data (quote_vty ~unfold:false def_vty) decls in
                let decls, body_fmt = go (extend_local ctx label.data def_vty) fmt_fields decls in
                decls, Bind (label.data, Pure (quote_vty ~unfold:false def_vty, def), body_fmt)

            (* x <- fmt; fmt_fields... *)
            | Bind_field (_, fmt) ->
                let fmt = check_format ctx fmt in
                let fmt_vty = eval_ty ctx (format_ty ctx fmt) in
                (* Add a new field to the record type *)
                let decls = Core.Label_map.add label.data (quote_vty ~unfold:false fmt_vty) decls in
                let decls, body_fmt = go (extend_local ctx label.data fmt_vty) fmt_fields decls in
                decls, Bind (label.data, fmt, body_fmt)
              end
      in

      let decls, fmt = go ctx fmt_fields Core.Label_map.empty in
      let ctx = extend_item ctx record_name (Record_type decls) in
      extend_item ctx name.data (Format_def fmt)

  | Format_def (name, tm) ->
      extend_item ctx name.data (Format_def (check_format ctx tm))

  | Type_def (name, tm) ->
      extend_item ctx name.data (Type_def (check_type ctx tm))

  | TermDef (name, ann, tm) ->
      begin match infer_ann ctx tm ann with
      | Kind_tm _ -> error name.loc "kind definitions are not supported"
      | Type_tm ty -> extend_item ctx name.data (Type_def ty)
      | Expr_tm (expr, ty) -> extend_item ctx name.data (Expr_def (quote_vty ~unfold:false ty, expr))
      | Format_tm fmt -> extend_item ctx name.data (Format_def fmt)
      end

let check_program (items : item list) : Core.program =
  let module String_map = Map.Make (String) in
  let module String_set = Set.Make (String) in

  let item_name (item : item) : string located =
    match item with
    | Record_type (name, _) -> name
    | Record_format (name, _) -> name
    | Format_def (name, _) -> name
    | Type_def (name, _) -> name
    | TermDef (name, _, _) -> name
  in

  let item_name_ids =
    Seq.fold_lefti
      (fun map i item ->
        let name = item_name item in
        if String_map.mem name.data map then
          error name.loc (Format.sprintf "the item name `%s` is defined multiple times" name.data)
        else
          String_map.add name.data i map)
      String_map.empty
      (List.to_seq items)
  in

  let rec tm_deps (locals : String_set.t) (tm : tm) : int list =
    match tm.data with
    | Name (name, args) ->
        let name =
          if String_set.mem name locals then [] else
            String_map.find_opt name item_name_ids |> Option.to_list
        in
        name @ List.concat_map (fun (Anon tm | Labelled (_, tm)) -> tm_deps locals tm) args
    | Placeholder -> []
    | Ann (tm, ann) ->
        tm_deps locals tm
          @ tm_deps locals ann
    | Let (name, None, def_tm, body_tm) ->
        tm_deps locals def_tm
          @ tm_deps (String_set.add name.data locals) body_tm
    | Let (name, Some def_ty, def_tm, body_tm) ->
        tm_deps locals def_ty
          @ tm_deps locals def_tm
          @ tm_deps (String_set.add name.data locals) body_tm
    | Bind (name, def_fmt, body_fmt) ->
        tm_deps locals def_fmt
          @ tm_deps (String_set.add name.data locals) body_fmt
    | Record_lit field_tms ->
        List.concat_map (fun (_, tm) -> tm_deps locals tm) field_tms
    | Int_lit _ -> []
    | Proj (tm, _) -> tm_deps locals tm
    | If_then_else (head, tm1, tm2) ->
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
      | Record_type (_, field_tms) ->
          id, List.concat_map (fun (_, tm) -> tm_deps String_set.empty tm) field_tms
      | Record_format (_, field_tms) ->
          let rec go locals (field_tms : format_field list) =
            match field_tms with
            | [] -> []
            | Let (label, None, tm) :: field_tms
            | Bind (label, tm) :: field_tms
            | Let_field (label, None, tm) :: field_tms
            | Bind_field (label, tm) :: field_tms ->
                tm_deps locals tm
                  @ go (String_set.add label.data locals) field_tms
            | Let (label,Some ann, tm) :: field_tms
            | Let_field (label,Some ann, tm) :: field_tms ->
                tm_deps locals ann
                  @ tm_deps locals tm
                  @ go (String_set.add label.data locals) field_tms
          in
          id, go String_set.empty field_tms
      | Format_def (_, tm) -> id, tm_deps String_set.empty tm
      | Type_def (_, tm) -> id, tm_deps String_set.empty tm
      | TermDef (_, None, tm) -> id, tm_deps String_set.empty tm
      | TermDef (_, Some ann, tm) -> id, tm_deps String_set.empty ann @ tm_deps String_set.empty tm
  in

  let rec go (ctx : context) (order : int list) =
    match order with
    | [] -> List.rev ctx.items
    | id :: order -> (go [@tailcall]) (check_item ctx (List.nth items id)) order
  in

  (* TODO: Sort with strongly connected components, elaborating to fixed-points *)
  match Tsort.sort (collect_deps items) with
  | Tsort.Sorted order ->
      go { items = []; locals = [] } order
        |> Core.zonk_program
  | Tsort.ErrorCycle _ -> failwith "TODO: cyclic items" (* TODO: raise a better error *)
