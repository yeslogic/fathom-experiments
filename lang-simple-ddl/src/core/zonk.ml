(** Zonking visits each node in the syntax tree, inlining any solved
    metavariables that are found. This is useful when we want to use the syntax
    for other things without having to refer back to the metacontext, for
    example when pretty printing or compiling programs.

    {1 Resources}

    - {{: https://stackoverflow.com/questions/31889048/what-does-the-ghc-source-mean-by-zonk}
      What does the GHC source mean by "zonk"?} on Stack overflow
    - {{: https://mail.haskell.org/pipermail/glasgow-haskell-users/2013-August/024209.html}
      Question about correct GHC-API use for type checking (or zonking, or tidying)}
      on the GHC mailing list
*)

open Syntax

let rec zonk_ty (ty : ty) : ty =
  match ty with
  | Item_var _ -> ty
  | Meta_var id ->
      begin match Semantics.Meta.lookup id with
      | Solved vty -> zonk_ty (Semantics.quote_vty ~unfold:false vty)
      | Unsolved -> ty
      end
  | List_type ty -> List_type (zonk_ty ty)
  | UInt8_type -> ty
  | UInt16_type -> ty
  | UInt32_type -> ty
  | UInt64_type -> ty
  | Int8_type -> ty
  | Int16_type -> ty
  | Int32_type -> ty
  | Int64_type -> ty
  | Bool_type -> ty
  | Format_repr format ->
      Format_repr (zonk_format format)

and zonk_expr (expr : expr) : expr =
  match expr with
  | Item_var _ -> expr
  | Local_var _ -> expr
  | Let (name, def_ty, def_expr, body_expr) ->
      let def_ty = zonk_ty def_ty
      and def_expr = zonk_expr def_expr
      and body_expr = zonk_expr body_expr in
      Let (name, def_ty, def_expr, body_expr)
  | Record_lit (name, field_exprs) ->
      Record_lit (name, field_exprs |> Label_map.map zonk_expr)
  | Record_proj (expr, label) ->
      Record_proj (zonk_expr expr, label)
  | List_lit exprs ->
      List_lit (exprs |> List.map zonk_expr)
  | UInt8_lit _ -> expr
  | UInt16_lit _ -> expr
  | UInt32_lit _ -> expr
  | UInt64_lit _ -> expr
  | Int8_lit _ -> expr
  | Int16_lit _ -> expr
  | Int32_lit _ -> expr
  | Int64_lit _ -> expr
  | Bool_lit _ -> expr
  | Bool_elim (head, expr1, expr2) ->
      let head = zonk_expr head
      and expr1 = zonk_expr expr1
      and expr2 = zonk_expr expr2 in
      Bool_elim (head, expr1, expr2)
  | Prim_app (prim, args) ->
      Prim_app (prim, args |> List.map zonk_expr)

and zonk_format (format : format) : format =
  match format with
  | Item_var _ -> format
  | Byte -> format
  | Repeat_len (len, elem_fmt) ->
      let len = zonk_expr len
      and elem_fmt = zonk_format elem_fmt in
      Repeat_len (len, elem_fmt)
  | Bind (name, def_fmt, body_fmt) ->
      let def_fmt = zonk_format def_fmt
      and body_fmt = zonk_format body_fmt in
      Bind (name, def_fmt, body_fmt)
  | Pure (ty, expr) ->
      let ty = zonk_ty ty
      and expr = zonk_expr expr in
      Pure (ty, expr)
  | Fail ty ->
      Fail (zonk_ty ty)
  | Bool_elim (head, format1, format2) ->
      let head = zonk_expr head
      and format1 = zonk_format format1
      and format2 = zonk_format format2 in
      Bool_elim (head, format1, format2)

let zonk_item (item : item) : item =
  match item with
  | Type_def ty ->
      Type_def (zonk_ty ty)
  | Record_type field_tys ->
      Record_type (field_tys |> Label_map.map zonk_ty)
  | Format_def format ->
      Format_def (zonk_format format)
  | Expr_def (ty, expr) ->
      let ty = zonk_ty ty
      and expr = zonk_expr expr in
      Expr_def (ty, expr)

let zonk_program (items : program) : program =
  items |> List.map @@ fun (name, item) ->
    (name, zonk_item item)
