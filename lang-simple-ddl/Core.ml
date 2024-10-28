(** Core language *)

type index = int

module LabelMap = Map.Make (String)

(** {1 Syntax} *)

type prim =
  | IntEq
  | IntNe
  | IntLe
  | IntLt
  | IntGe
  | IntGt
  | IntNeg
  | IntAdd
  | IntSub
  | IntMul
  | IntDiv
  | IntLogicalNot
  | IntLogicalAnd
  | IntLogicalOr
  | IntLogicalXor
  | IntLogicalShl
  | IntArithShr
  | IntLogicalShr

type ty =
  | ItemVar of string
  | ListType of ty
  | IntType
  | BoolType
  (* TODO: | Repr of format *)

type expr =
  | ItemVar of string
  | LocalVar of index
  | Let of string * ty * expr * expr
  | RecordLit of string * expr LabelMap.t
  | RecordProj of expr * string
  | ListLit of expr list
  | IntLit of int
  | BoolLit of bool
  | BoolElim of expr * expr * expr
  | PrimApp of prim * expr list

type format =
  | ItemVar of string
  | Byte
  | RepeatLen of expr * format
  | Bind of string * format * format
  | Pure of ty * expr
  | Fail of ty
  | BoolElim of expr * format * format

(** Top-level items *)
type item =
  | TypeDef of ty
  | RecordType of ty LabelMap.t
  | FormatDef of format
  | ExprDef of ty * expr

type program =
  (string * item) list


(** {2 Pretty printing} *)

let rec pp_print_ty ppf (t : ty) =
  match t with
  | t -> pp_print_atomic_ty ppf t

and pp_print_atomic_ty ppf t =
  match t with
  | ItemVar name -> Format.pp_print_string ppf name
  | ListType elem_ty -> Format.fprintf ppf "List(%a)" pp_print_ty elem_ty
  | IntType -> Format.fprintf ppf "Int"
  | BoolType -> Format.fprintf ppf "Bool"
  (* | ty -> Format.fprintf ppf "(%a)" pp_print_ty ty *)


module Semantics = struct

  (** {1 Semantic domain} *)

  type vty =
    | ItemVar of string * vty Lazy.t
    | RecordType of string * vty LabelMap.t
    | ListType of vty
    | IntType
    | BoolType

  (** Top-level items are “glued”, meaning that we remember the item they refer to. See.
      {{:https://github.com/AndrasKovacs/elaboration-zoo/blob/master/GluedEval.hs} GluedEval.hs}
      for more information on this technique.
  *)

  type vexpr =
    | IntLit of int
    | BoolLit of bool
    | ListLit of vexpr list
    | RecordLit of string * vexpr LabelMap.t
    | TupleLit of vexpr list

  type env =
    vexpr list

  let rec force_vty (vty : vty) : vty =
    match vty with
    | ItemVar (_, vty) -> (force_vty [@tailcall]) (Lazy.force vty)
    | vty -> vty


  (** {1 Evaluation} *)

  let rec eval_ty (items : program) (ty : ty) : vty =
    match ty with
    | ItemVar name ->
        let vty () : vty =
          match List.assoc name items with
          | TypeDef ty -> eval_ty items ty
          | RecordType decls ->
              RecordType (name, LabelMap.map (eval_ty items) decls)
          | FormatDef _ | ExprDef (_, _) -> failwith "type expected"
        in
        ItemVar (name, Lazy.from_fun vty)
    | ListType elem_ty -> ListType (eval_ty items elem_ty)
    | IntType -> IntType
    | BoolType -> BoolType

  let prim_app (prim : prim) : vexpr list -> vexpr =
    match prim with
    | IntEq -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> BoolLit (Int.equal x y)
    | IntNe -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> BoolLit (not (Int.equal x y))
    | IntLe -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> BoolLit (x <= y)
    | IntLt -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> BoolLit (x < y)
    | IntGt -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> BoolLit (x < y)
    | IntGe -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> BoolLit (x >= y)
    | IntNeg -> fun[@warning "-partial-match"] [IntLit x] -> IntLit (Int.neg x)
    | IntAdd -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> IntLit (Int.add x y)
    | IntSub -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> IntLit (Int.sub x y)
    | IntMul -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> IntLit (Int.mul x y)
    | IntDiv -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> IntLit (Int.div x y)
    | IntLogicalNot -> fun[@warning "-partial-match"] [IntLit x] -> IntLit (Int.lognot x)
    | IntLogicalAnd -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> IntLit (Int.logand x y)
    | IntLogicalOr -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> IntLit (Int.logor x y)
    | IntLogicalXor -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> IntLit (Int.logxor x y)
    | IntLogicalShl -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> IntLit (Int.shift_left x y)
    | IntArithShr -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> IntLit (Int.shift_right x y)
    | IntLogicalShr -> fun[@warning "-partial-match"] [IntLit x; IntLit y] -> IntLit (Int.shift_right_logical x y)

  let rec eval_expr (items : program) (locals : env) (expr : expr) : vexpr =
    match expr with
    | ItemVar name ->
        begin match List.assoc name items with
        | ExprDef (_, def) -> eval_expr items locals def
        | _ -> invalid_arg "not a format item"
        end
    | LocalVar index -> List.nth locals index
    | Let (_, _, def, body) ->
        let def = eval_expr items locals def in
        eval_expr items (def :: locals) body
    | RecordLit (name, field_exprs) ->
        RecordLit (name, LabelMap.map (eval_expr items locals) field_exprs)
    | RecordProj (head, label) ->
        begin match eval_expr items locals head with
        | RecordLit (_, field_vexprs) -> LabelMap.find label field_vexprs
        | _ -> failwith "record expected"
        end
    | ListLit exprs ->
        ListLit (List.map (eval_expr items locals) exprs)
    | IntLit i -> IntLit i
    | BoolLit b -> BoolLit b
    | BoolElim (head, expr1, expr2) ->
        begin match eval_expr items locals head with
        | BoolLit true -> eval_expr items locals expr1
        | BoolLit false -> eval_expr items locals expr2
        | _ -> failwith "boolean expected"
        end
    | PrimApp (prim, args) ->
        prim_app prim (List.map (eval_expr items locals) args)

  let rec format_ty (items : program) (fmt : format) : ty =
    match fmt with
    | ItemVar name ->
        begin match List.assoc name items with
        | FormatDef fmt -> format_ty items fmt
        | _ -> invalid_arg "expected format"
        end
    | Byte -> IntType
    | RepeatLen (_, elem_fmt) -> ListType (format_ty items elem_fmt)
    | Bind (_, _, body_fmt) -> format_ty items body_fmt
    | Pure (ty, _) -> ty
    | Fail ty -> ty
    | BoolElim (_, fmt1, _) -> format_ty items fmt1


  (** {1 Quotation} *)

  let rec quote_vty ?(unfold_items=false) (vty : vty) : ty =
    match vty with
    | ItemVar (_, vty) when unfold_items -> quote_vty ~unfold_items (Lazy.force vty)
    | ItemVar (name, _) -> ItemVar name
    | RecordType (name, _) -> ItemVar name
    | ListType vty -> ListType (quote_vty ~unfold_items vty)
    | IntType -> IntType
    | BoolType -> BoolType


  (** {1 Unification} *)

  exception FailedToUnify

  let rec unify_vtys (items : program) (vty1 : vty) (vty2 : vty) =
    match vty1, vty2 with
    | ItemVar (name1, _), ItemVar (name2, _) when name1 = name2 -> ()
    | ItemVar (_, vty1), vty2 -> unify_vtys items (Lazy.force vty1) vty2
    | vty1, ItemVar (_, vty2) -> unify_vtys items vty1 (Lazy.force vty2)
    | RecordType (name1, _), RecordType (name2, _) when name1 = name2 -> ()
    | ListType elem_vty1, ListType elem_vty2 -> unify_vtys items elem_vty1 elem_vty2
    | IntType, IntType -> ()
    | BoolType, BoolType -> ()
    | _ -> raise FailedToUnify


  (** {2 Decode semantics} *)

  type 'a decoder = input:bytes -> pos:int -> int * 'a

  exception DecodeFailure of int

  let decode_format (items : program) (fmt : format) : vexpr decoder =
    let rec decode_format (locals : env) (fmt : format) : vexpr decoder =
      fun ~input ~pos ->
        match fmt with
        | ItemVar name ->
            begin match List.assoc name items with
            | FormatDef fmt -> decode_format [] fmt ~input ~pos
            | _ -> invalid_arg "not a format item"
            end
        | Byte ->
            decode_byte ~input ~pos
        | RepeatLen (len, elem_fmt) ->
            begin match eval_expr items locals len with
            | IntLit len ->
                let pos, vexprs = decode_elems locals len elem_fmt ~input ~pos in
                pos, ListLit vexprs
            | _ -> failwith "integer expected"
            end
        | Bind (_, def_fmt, body_fmt) ->
            let pos, def = decode_format locals def_fmt ~input ~pos in
            decode_format (def :: locals) body_fmt ~input ~pos
        | Pure (_, expr) -> pos, eval_expr items locals expr
        | Fail _ -> raise (DecodeFailure pos)
        | BoolElim (head, fmt1, fmt2) ->
            begin match eval_expr items locals head with
            | BoolLit true -> decode_format locals fmt1 ~input ~pos
            | BoolLit false -> decode_format locals fmt2 ~input ~pos
            | _ -> failwith "boolean expected"
            end

    and decode_byte : vexpr decoder =
      fun ~input ~pos ->
        if pos < Bytes.length input then
          pos + 1, IntLit (int_of_char (Bytes.unsafe_get input pos))
        else
          raise (DecodeFailure pos)

    and decode_elems (locals : env) (len : int) (elem_fmt : format) : vexpr list decoder =
      fun ~input ~pos ->
        match len with
        | 0 -> pos, []
        | len ->
            let pos, vexpr = decode_format locals elem_fmt ~input ~pos in
            let pos, vexprs = decode_elems locals (len - 1) elem_fmt ~input ~pos in
            pos, vexpr :: vexprs
    in

    decode_format [] fmt

end

module Compile = struct

  (* TODO: Name avoidance *)

  module StringMap = Map.Make (String)

  type source_context = {
    items : program;
    target_names : string StringMap.t;
  }

  let rec compile_ty (src_ctx : source_context) (ty : ty) : Rust.ty =
    match ty with
    | ItemVar name ->
        Path ([StringMap.find name src_ctx.target_names], [])
    | ListType ty ->
        Path (["Vec"], [compile_ty src_ctx ty])
    | IntType ->
        Path (["i64"], []) (* TODO: More integer types *)
    | BoolType ->
        Path (["bool"], [])

  let rec compile_stmts (src_ctx : source_context) (locals : string list) (expr : expr) : Rust.stmts =
    match expr with
    | Let (name, def_ty, def, body) ->
        let name = CaseConv.quiet_snake_case name in
        let ty = compile_ty src_ctx def_ty in
        let expr = compile_expr src_ctx locals def in
        let stmts, body_expr = compile_stmts src_ctx (name :: locals) body in
        Let (name, Some ty, expr) :: stmts, body_expr
    | expr ->
        [], Some (compile_expr src_ctx locals expr)

  and compile_expr (src_ctx : source_context) (locals : string list) (expr : expr) : Rust.expr =
    match expr with
    | ItemVar name ->
        (* FIXME: Check if constant *)
        Path [StringMap.find name src_ctx.target_names]
    | LocalVar index ->
        Path [List.nth locals index]
    | Let _ ->
        Block (compile_stmts src_ctx locals expr)
    | RecordLit (name, field_exprs) ->
        let name = StringMap.find name src_ctx.target_names in
        let fields =
          StringMap.to_seq field_exprs
          |> Seq.map (fun (label, expr) ->
              CaseConv.quiet_snake_case label, (* TODO: handle this better? *)
              compile_expr src_ctx locals expr)
          |> List.of_seq
        in
        StructLit (name, fields)
    | RecordProj (head, label) ->
        StructProj (
          compile_expr src_ctx locals head,
          CaseConv.quiet_snake_case label (* TODO: handle this better? *)
        )
    | ListLit exprs ->
        VecLit (List.map (compile_expr src_ctx locals) exprs)
    | IntLit i -> I64Lit i
    | BoolLit b -> BoolLit b
    | BoolElim (head, expr1, expr2) ->
        IfElse (
          compile_expr src_ctx locals head,
          compile_stmts src_ctx locals expr1,
          compile_stmts src_ctx locals expr2
        )
    | PrimApp (prim, args) ->
        let prefix op args : Rust.expr =
          match args with
          | [x] -> PrefixOp (op, compile_expr src_ctx locals x)
          | _ -> failwith "invalid prim"
        and infix op args : Rust.expr =
          match args with
          | [x; y] -> InfixOp (compile_expr src_ctx locals x, op, compile_expr src_ctx locals y)
          | _ -> failwith "invalid prim"
        in
        match prim with
        (* See https://doc.rust-lang.org/reference/expressions/operator-expr.html
           for the semantics of Rust’s binary operators *)
        | IntEq -> infix "==" args
        | IntNe -> infix "!=" args
        | IntLe -> infix "<=" args
        | IntLt -> infix "<" args
        | IntGt -> infix ">" args
        | IntGe -> infix ">=" args
        | IntNeg -> prefix "-" args
        | IntAdd -> infix "+" args
        | IntSub -> infix "-" args
        | IntMul -> infix "*" args
        | IntDiv -> infix "/" args
        | IntLogicalNot -> prefix "!" args
        | IntLogicalAnd -> infix "&" args
        | IntLogicalOr -> infix "|" args
        | IntLogicalXor -> infix "^" args
        | IntLogicalShl -> infix "<<" args
        | IntArithShr -> infix ">>" args
        | IntLogicalShr -> Path ["todo!(\"logical shift right\")"] (* FIXME *)

  let rec compile_format_stmts (src_ctx : source_context) (locals : string list) (fmt : format) : Rust.stmts =
    match fmt with
    | Bind (name, Pure (def_ty, def), body_fmt) ->
        let name = CaseConv.quiet_snake_case name in
        let ty = compile_ty src_ctx def_ty in
        let expr = compile_expr src_ctx locals def in
        let stmts, body_expr = compile_format_stmts src_ctx (name :: locals) body_fmt in
        Let (name, Some ty, expr) :: stmts, body_expr

    | Bind (name, def_fmt, body_fmt) ->
        let name = CaseConv.quiet_snake_case name in
        let expr = compile_format_expr src_ctx locals def_fmt in
        let stmts, body_expr = compile_format_stmts src_ctx (name :: locals) body_fmt in
        Let (name, None, PostfixOp (expr, "?")) :: stmts, body_expr

    | fmt ->
        [], Some (compile_format_expr src_ctx locals fmt)

  and compile_format_expr (src_ctx : source_context) (locals : string list) (fmt : format) : Rust.expr =
    match fmt with
    | ItemVar name ->
        let name = StringMap.find name src_ctx.target_names in
        Call (Path [name], [Path ["input"]; Path ["pos"]])
    | Byte ->
        Call (Path ["read_byte"], [Path ["input"]; Path ["pos"]])
    | RepeatLen (len, elem_fmt) ->
        let elem_ty = Semantics.format_ty src_ctx.items fmt in
        RepeatCount (
          compile_expr src_ctx locals len,
          compile_format_expr src_ctx locals elem_fmt,
          Path (["Result"], [compile_ty src_ctx elem_ty; Placeholder])
        )
    (* Optimisation for let-bound formats *)
    | Bind _ ->
        Block (compile_format_stmts src_ctx locals fmt)
    | Pure (_, expr) ->
        Call (Path ["Ok"], [compile_expr src_ctx locals expr])
    | Fail _ ->
        Call (Path ["Err"], [UnitLit])
    | BoolElim (head, fmt1, fmt2) ->
        IfElse (
          compile_expr src_ctx locals head,
          compile_format_stmts src_ctx locals fmt1,
          compile_format_stmts src_ctx locals fmt2
        )

  let compile_item (src_ctx : source_context) (name, item : string * item) : Rust.item =
    let name = StringMap.find name src_ctx.target_names in

    match item with
    | TypeDef ty ->
        Type (name, compile_ty src_ctx ty)

    | RecordType field_tys ->
        let fields =
          StringMap.to_seq field_tys
          |> Seq.map (fun (label, ty) ->
            CaseConv.quiet_snake_case label, (* TODO: handle this better? *)
            compile_ty src_ctx ty)
          |> List.of_seq
        in
        Struct (name, fields)

    | FormatDef fmt ->
        let input_ty : Rust.ty = Ref (Slice (Path (["u8"], []))) in
        let pos_ty : Rust.ty = RefMut (Path (["usize"], [])) in
        let fmt_ty : Rust.ty = compile_ty src_ctx (Semantics.format_ty src_ctx.items fmt) in
        let ret_ty : Rust.ty = Path (["Result"], [fmt_ty; Unit]) in
        let body = compile_format_stmts src_ctx [] fmt in

        Fn (name, ["input", input_ty; "pos", pos_ty], ret_ty, body)

    | ExprDef (def_ty, def) ->
        let ty = compile_ty src_ctx def_ty in
        (* FIXME: check if the expression is a valid constant *)
        let expr = compile_expr src_ctx [] def in
        Const (name, ty, expr)

  let compile_program (items : program) : Rust.item list =
    let target_names =
      (* Assign top-level item names in target program *)
      List.fold_left
        (fun acc (name, item) ->
          match item with
          | TypeDef _ | RecordType _ -> StringMap.add name (CaseConv.pascal_case name) acc
          | FormatDef _ -> StringMap.add name ("read_" ^ CaseConv.quiet_snake_case name) acc
          | ExprDef _ -> StringMap.add name (CaseConv.screaming_snake_case name) acc)
        StringMap.empty
        items
    in
    List.map (compile_item { items; target_names }) items

end
