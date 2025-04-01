open Syntax

(* TODO: Name avoidance *)

module String_set = Set.Make (String)
module String_map = Map.Make (String)

type context = {
  source_items : program;
  target_item_names : string String_map.t;
  target_items : Rust.item list;
  target_local_names : string list;
  target_deps : String_set.t ref;
}

let empty_context (source_items : program) : context = {
  source_items;
  target_item_names = String_map.empty;
  target_items = [];
  target_local_names = [];
  target_deps = ref String_set.empty;
}

let extend_source_local (ctx : context) (tgt_name : string) =
  { ctx with target_local_names = tgt_name :: ctx.target_local_names }

let compile_item_var (ctx : context) (name : string) : string =
  String_map.find name ctx.target_item_names

let rec compile_ty (ctx : context) (ty : ty) : Rust.ty =
  match ty with
  | Item_var name -> Path ([compile_item_var ctx name], [])
  | Meta_var id -> failwith (Printf.sprintf "compile_ty: encountered meta variable `$%i`" id)
  | List_type ty -> Path (["Vec"], [compile_ty ctx ty])
  | UInt8_type -> Path (["u8"], [])
  | UInt16_type -> Path (["u16"], [])
  | UInt32_type -> Path (["u32"], [])
  | UInt64_type -> Path (["u64"], [])
  | Int8_type -> Path (["i8"], [])
  | Int16_type -> Path (["i16"], [])
  | Int32_type -> Path (["i32"], [])
  | Int64_type -> Path (["i64"], [])
  | Bool_type -> Path (["bool"], [])
  | Format_repr fmt ->
      compile_ty ctx (Semantics.format_ty ctx.source_items fmt)

let rec compile_stmts (ctx : context) (expr : expr) : Rust.stmts =
  match expr with
  | Let (name, def_ty, def, body) ->
      let name = Case_conv.quiet_snake_case name in
      let ty = compile_ty ctx def_ty in
      let expr = compile_expr ctx def in
      let stmts, body_expr = compile_stmts (extend_source_local ctx name) body in
      Let (name, Some ty, expr) :: stmts, body_expr
  | expr ->
      [], Some (compile_expr ctx expr)

and compile_expr (ctx : context) (expr : expr) : Rust.expr =
  match expr with
  | Item_var name ->
      (* FIXME: Check if constant *)
      Path [compile_item_var ctx name]
  | Local_var index ->
      Path [List.nth ctx.target_local_names index]
  | Let _ ->
      Block (compile_stmts ctx expr)
  | Record_lit (name, field_exprs) ->
      let name = compile_item_var ctx name in
      (* TODO: lookup field mappings *)
      let fields =
        String_map.to_seq field_exprs
        |> Seq.map (fun (label, expr) ->
            Case_conv.quiet_snake_case label, (* TODO: handle this better? *)
            compile_expr ctx expr)
        |> List.of_seq
      in
      Struct_lit (name, fields)
  | Record_proj (head, label) ->
      (* TODO: get the type of the head *)
      (* TODO: lookup field mappings *)
      Struct_proj (
        compile_expr ctx head,
        Case_conv.quiet_snake_case label (* TODO: handle this better? *)
      )
  | List_lit exprs ->
      Vec_lit (List.map (compile_expr ctx) exprs)
  | UInt8_lit i -> U8_lit i
  | UInt16_lit i -> U16_lit i
  | UInt32_lit i -> U32_lit i
  | UInt64_lit i -> U64_lit i
  | Int8_lit i -> I8_lit i
  | Int16_lit i -> I16_lit i
  | Int32_lit i -> I32_lit i
  | Int64_lit i -> I64_lit i
  | Bool_lit b -> Bool_lit b
  | Bool_elim (head, expr1, expr2) ->
      If_else (
        compile_expr ctx head,
        compile_stmts ctx expr1,
        compile_stmts ctx expr2
      )
  | Prim_app (prim, args) ->
      match prim, args with
      | Eq _, [x; y] -> Infix_op (compile_expr ctx x, "==", compile_expr ctx y)
      | Ne _, [x; y] -> Infix_op (compile_expr ctx x, "!=", compile_expr ctx y)
      | Le _, [x; y] -> Infix_op (compile_expr ctx x, "<=", compile_expr ctx y)
      | Lt _, [x; y] -> Infix_op (compile_expr ctx x, "<", compile_expr ctx y)
      | Gt _, [x; y] -> Infix_op (compile_expr ctx x, ">", compile_expr ctx y)
      | Ge _, [x; y] -> Infix_op (compile_expr ctx x, ">=", compile_expr ctx y)
      | Neg _, [x] -> Prefix_op ("-", compile_expr ctx x)
      | Add _, [x; y] -> Infix_op (compile_expr ctx x, "+", compile_expr ctx y)
      | Sub _, [x; y] -> Infix_op (compile_expr ctx x, "-", compile_expr ctx y)
      | Mul _, [x; y] -> Infix_op (compile_expr ctx x, "*", compile_expr ctx y)
      | Div _, [x; y] -> Infix_op (compile_expr ctx x, "/", compile_expr ctx y)
      | Bit_not _, [x] -> Prefix_op ("!", compile_expr ctx x)
      | Bit_and _, [x; y] -> Infix_op (compile_expr ctx x, "&", compile_expr ctx y)
      | Bit_or _, [x; y] -> Infix_op (compile_expr ctx x, "|", compile_expr ctx y)
      | Bit_xor _, [x; y] -> Infix_op (compile_expr ctx x, "^", compile_expr ctx y)
      | Bit_shl _, [x; y] -> Infix_op (compile_expr ctx x, "<<", compile_expr ctx y)
      | Bit_shr _, [x; y] -> Infix_op (compile_expr ctx x, ">>", compile_expr ctx y)
      | _ -> failwith "invalid prim"

let rec compile_format_stmts (ctx : context) (fmt : format) : Rust.stmts =
  match fmt with
  (* Optimisation for let-bound formats *)
  | Bind (name, Pure (def_ty, def), body_fmt) ->
      let name = Case_conv.quiet_snake_case name in
      let ty = compile_ty ctx def_ty in
      let expr = compile_expr ctx def in
      let stmts, body_expr = compile_format_stmts (extend_source_local ctx name) body_fmt in
      Let (name, Some ty, expr) :: stmts, body_expr

  | Bind (name, def_fmt, body_fmt) ->
      let name = Case_conv.quiet_snake_case name in
      let expr = compile_format_expr ctx def_fmt in
      let stmts, body_expr = compile_format_stmts (extend_source_local ctx name) body_fmt in
      Let (name, None, Postfix_op (expr, "?")) :: stmts, body_expr

  | fmt ->
      [], Some (compile_format_expr ctx fmt)

and compile_format_expr (ctx : context) (fmt : format) : Rust.expr =
  match fmt with
  | Item_var _ | Byte | Repeat_len _ ->
      Call (compile_format_fun_expr ctx fmt, [Path ["input"]; Path ["pos"]])
  | Bind _ ->
      Block (compile_format_stmts ctx fmt)
  | Pure (_, expr) ->
      Call (Path ["Ok"], [compile_expr ctx expr])
  | Fail _ ->
      Call (Path ["Err"], [Unit_lit])
  | Bool_elim (head, fmt1, fmt2) ->
      If_else (
        compile_expr ctx head,
        compile_format_stmts ctx fmt1,
        compile_format_stmts ctx fmt2
      )

and compile_format_fun_expr (ctx : context) (fmt : format) : Rust.expr =
  match fmt with
  | Item_var name ->
      Path [compile_item_var ctx name]
  | Byte ->
      ctx.target_deps := String_set.add "read_byte" !(ctx.target_deps);
      Path ["read_byte"]
  | Repeat_len (len, elem_fmt) ->
      ctx.target_deps := String_set.add "read_repeat_len" !(ctx.target_deps);
      Call (Path ["read_repeat_len"], [compile_expr ctx len; compile_format_fun_expr ctx elem_fmt])
  | fmt ->
      Closure (["input"; "pos"], compile_format_expr ctx fmt)

let compile_item (ctx : context) (name, item : string * item) : string * Rust.item =
  match item with
  | Type_def ty ->
      let name = Case_conv.pascal_case name in
      name, Type (name, compile_ty ctx ty)

  | Record_type field_tys ->
      let name = Case_conv.pascal_case name in
      let fields =
        String_map.to_seq field_tys
        |> Seq.map (fun (label, ty) ->
          Case_conv.quiet_snake_case label, (* TODO: handle this better? *)
          compile_ty ctx ty)
        |> List.of_seq
      in
      name, Struct (name, fields)

  | Format_def fmt ->
      let name = "read_" ^ Case_conv.quiet_snake_case name in
      let item : Rust.item =
        Fn (
          name,
          [
            "input", Ref (Slice (Path (["u8"], [])));
            "pos", Ref_mut (Path (["usize"], []));
          ],
          Path (["Result"], [compile_ty ctx (Semantics.format_ty ctx.source_items fmt); Unit]),
          compile_format_stmts ctx fmt
        )
      in

      name, item

  | Expr_def (def_ty, def) ->
      let name = Case_conv.screaming_snake_case name in
      let ty = compile_ty ctx def_ty in
      (* FIXME: check if the expression is a valid constant *)
      let expr = compile_expr ctx def in
      name, Const (name, ty, expr)

let compile_program (items : program) : Rust.program =
  let ctx =
    items |> ListLabels.fold_left
      ~init:(empty_context items)
      ~f:(fun ctx (name, item) ->
        let target_name, target_item =
          compile_item ctx (name, item)
        in
        { ctx with
          target_item_names = String_map.add name target_name ctx.target_item_names;
          target_items = target_item :: ctx.target_items;
        })
  in
  {
    deps = !(ctx.target_deps) |> String_set.to_list;
    items = ctx.target_items |> List.rev;
  }
