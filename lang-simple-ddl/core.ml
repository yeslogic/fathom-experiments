(** Core language *)

type index = int

module Label_map = Map.Make (String)

(** {1 Syntax} *)

type prim =
  | Int_eq
  | Int_ne
  | Int_le
  | Int_lt
  | Int_ge
  | Int_gt
  | Int_neg
  | Int_add
  | Int_sub
  | Int_mul
  | Int_div
  | Int_logical_not
  | Int_logical_and
  | Int_logical_or
  | Int_logical_xor
  | Int_logical_shl
  | Int_arith_shr
  | Int_logical_shr

type ty =
  | Item_var of string
  | List_type of ty
  | Int_type
  | Bool_type
  (* TODO: | Repr of format *)

type expr =
  | Item_var of string
  | Local_var of index
  | Let of string * ty * expr * expr
  | Record_lit of string * expr Label_map.t
  | Record_proj of expr * string
  | List_lit of expr list
  | Int_lit of int
  | Bool_lit of bool
  | Bool_elim of expr * expr * expr
  | Prim_app of prim * expr list

type format =
  | Item_var of string
  | Byte
  | Repeat_len of expr * format
  | Bind of string * format * format
  | Pure of ty * expr
  | Fail of ty
  | Bool_elim of expr * format * format

(** Top-level items *)
type item =
  | Type_def of ty
  | Record_type of ty Label_map.t
  | Format_def of format
  | Expr_def of ty * expr

type program =
  (string * item) list


(** {2 Pretty printing} *)

let rec pp_print_ty ppf (t : ty) =
  match t with
  | t -> pp_print_atomic_ty ppf t

and pp_print_atomic_ty ppf t =
  match t with
  | Item_var name -> Format.pp_print_string ppf name
  | List_type elem_ty -> Format.fprintf ppf "List(%a)" pp_print_ty elem_ty
  | Int_type -> Format.fprintf ppf "Int"
  | Bool_type -> Format.fprintf ppf "Bool"
  (* | ty -> Format.fprintf ppf "(%a)" pp_print_ty ty *)


module Semantics = struct

  (** {1 Semantic domain} *)

  type vty =
    | Item_var of string * vty Lazy.t
    | Record_type of string * vty Label_map.t
    | List_type of vty
    | Int_type
    | Bool_type

  (** Top-level items are “glued”, meaning that we remember the item they refer to. See.
      {{:https://github.com/AndrasKovacs/elaboration-zoo/blob/master/GluedEval.hs} GluedEval.hs}
      for more information on this technique.
  *)

  type vexpr =
    | Int_lit of int
    | Bool_lit of bool
    | List_lit of vexpr list
    | Record_lit of string * vexpr Label_map.t
    | TupleLit of vexpr list

  type env =
    vexpr list

  let rec force_vty (vty : vty) : vty =
    match vty with
    | Item_var (_, vty) -> (force_vty [@tailcall]) (Lazy.force vty)
    | vty -> vty


  (** {1 Evaluation} *)

  let rec eval_ty (items : program) (ty : ty) : vty =
    match ty with
    | Item_var name ->
        let vty () : vty =
          match List.assoc name items with
          | Type_def ty -> eval_ty items ty
          | Record_type decls ->
              Record_type (name, Label_map.map (eval_ty items) decls)
          | Format_def _ | Expr_def (_, _) -> failwith "type expected"
        in
        Item_var (name, Lazy.from_fun vty)
    | List_type elem_ty -> List_type (eval_ty items elem_ty)
    | Int_type -> Int_type
    | Bool_type -> Bool_type

  let prim_app (prim : prim) : vexpr list -> vexpr =
    match prim with
    | Int_eq -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Bool_lit (Int.equal x y)
    | Int_ne -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Bool_lit (not (Int.equal x y))
    | Int_le -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Bool_lit (x <= y)
    | Int_lt -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Bool_lit (x < y)
    | Int_gt -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Bool_lit (x < y)
    | Int_ge -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Bool_lit (x >= y)
    | Int_neg -> fun[@warning "-partial-match"] [Int_lit x] -> Int_lit (Int.neg x)
    | Int_add -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Int_lit (Int.add x y)
    | Int_sub -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Int_lit (Int.sub x y)
    | Int_mul -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Int_lit (Int.mul x y)
    | Int_div -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Int_lit (Int.div x y)
    | Int_logical_not -> fun[@warning "-partial-match"] [Int_lit x] -> Int_lit (Int.lognot x)
    | Int_logical_and -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Int_lit (Int.logand x y)
    | Int_logical_or -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Int_lit (Int.logor x y)
    | Int_logical_xor -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Int_lit (Int.logxor x y)
    | Int_logical_shl -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Int_lit (Int.shift_left x y)
    | Int_arith_shr -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Int_lit (Int.shift_right x y)
    | Int_logical_shr -> fun[@warning "-partial-match"] [Int_lit x; Int_lit y] -> Int_lit (Int.shift_right_logical x y)

  let rec eval_expr (items : program) (locals : env) (expr : expr) : vexpr =
    match expr with
    | Item_var name ->
        begin match List.assoc name items with
        | Expr_def (_, def) -> eval_expr items locals def
        | _ -> invalid_arg "not a format item"
        end
    | Local_var index -> List.nth locals index
    | Let (_, _, def, body) ->
        let def = eval_expr items locals def in
        eval_expr items (def :: locals) body
    | Record_lit (name, field_exprs) ->
        Record_lit (name, Label_map.map (eval_expr items locals) field_exprs)
    | Record_proj (head, label) ->
        begin match eval_expr items locals head with
        | Record_lit (_, field_vexprs) -> Label_map.find label field_vexprs
        | _ -> failwith "record expected"
        end
    | List_lit exprs ->
        List_lit (List.map (eval_expr items locals) exprs)
    | Int_lit i -> Int_lit i
    | Bool_lit b -> Bool_lit b
    | Bool_elim (head, expr1, expr2) ->
        begin match eval_expr items locals head with
        | Bool_lit true -> eval_expr items locals expr1
        | Bool_lit false -> eval_expr items locals expr2
        | _ -> failwith "boolean expected"
        end
    | Prim_app (prim, args) ->
        prim_app prim (List.map (eval_expr items locals) args)

  let rec format_ty (items : program) (fmt : format) : ty =
    match fmt with
    | Item_var name ->
        begin match List.assoc name items with
        | Format_def fmt -> format_ty items fmt
        | _ -> invalid_arg "expected format"
        end
    | Byte -> Int_type
    | Repeat_len (_, elem_fmt) -> List_type (format_ty items elem_fmt)
    | Bind (_, _, body_fmt) -> format_ty items body_fmt
    | Pure (ty, _) -> ty
    | Fail ty -> ty
    | Bool_elim (_, fmt1, _) -> format_ty items fmt1


  (** {1 Quotation} *)

  let rec quote_vty ?(unfold_items=false) (vty : vty) : ty =
    match vty with
    | Item_var (_, vty) when unfold_items -> quote_vty ~unfold_items (Lazy.force vty)
    | Item_var (name, _) -> Item_var name
    | Record_type (name, _) -> Item_var name
    | List_type vty -> List_type (quote_vty ~unfold_items vty)
    | Int_type -> Int_type
    | Bool_type -> Bool_type


  (** {1 Unification} *)

  exception Failed_to_unify

  let rec unify_vtys (items : program) (vty1 : vty) (vty2 : vty) =
    match vty1, vty2 with
    | Item_var (name1, _), Item_var (name2, _) when name1 = name2 -> ()
    | Item_var (_, vty1), vty2 -> unify_vtys items (Lazy.force vty1) vty2
    | vty1, Item_var (_, vty2) -> unify_vtys items vty1 (Lazy.force vty2)
    | Record_type (name1, _), Record_type (name2, _) when name1 = name2 -> ()
    | List_type elem_vty1, List_type elem_vty2 -> unify_vtys items elem_vty1 elem_vty2
    | Int_type, Int_type -> ()
    | Bool_type, Bool_type -> ()
    | _ -> raise Failed_to_unify


  (** {2 Decode semantics} *)

  type 'a decoder = input:bytes -> pos:int -> int * 'a

  exception Decode_failure of int

  let decode_format (items : program) (fmt : format) : vexpr decoder =
    let rec decode_format (locals : env) (fmt : format) : vexpr decoder =
      fun ~input ~pos ->
        match fmt with
        | Item_var name ->
            begin match List.assoc name items with
            | Format_def fmt -> decode_format [] fmt ~input ~pos
            | _ -> invalid_arg "not a format item"
            end
        | Byte ->
            decode_byte ~input ~pos
        | Repeat_len (len, elem_fmt) ->
            begin match eval_expr items locals len with
            | Int_lit len ->
                let pos, vexprs = decode_elems locals len elem_fmt ~input ~pos in
                pos, List_lit vexprs
            | _ -> failwith "integer expected"
            end
        | Bind (_, def_fmt, body_fmt) ->
            let pos, def = decode_format locals def_fmt ~input ~pos in
            decode_format (def :: locals) body_fmt ~input ~pos
        | Pure (_, expr) -> pos, eval_expr items locals expr
        | Fail _ -> raise (Decode_failure pos)
        | Bool_elim (head, fmt1, fmt2) ->
            begin match eval_expr items locals head with
            | Bool_lit true -> decode_format locals fmt1 ~input ~pos
            | Bool_lit false -> decode_format locals fmt2 ~input ~pos
            | _ -> failwith "boolean expected"
            end

    and decode_byte : vexpr decoder =
      fun ~input ~pos ->
        if pos < Bytes.length input then
          pos + 1, Int_lit (int_of_char (Bytes.unsafe_get input pos))
        else
          raise (Decode_failure pos)

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

  module String_map = Map.Make (String)

  type context = {
    source_items : program;
    target_item_names : string String_map.t;
    target_items : Rust.item list;
    target_local_names : string list;
  }

  let empty_context (source_items : program) : context = {
    source_items;
    target_item_names = String_map.empty;
    target_items = [];
    target_local_names = [];
  }

  let extend_source_local (ctx : context) (tgt_name : string) =
    { ctx with target_local_names = tgt_name :: ctx.target_local_names }

  let compile_item_var (ctx : context) (name : string) : string =
    String_map.find name ctx.target_item_names

  let rec compile_ty (ctx : context) (ty : ty) : Rust.ty =
    match ty with
    | Item_var name ->
        Path ([compile_item_var ctx name], [])
    | List_type ty ->
        Path (["Vec"], [compile_ty ctx ty])
    | Int_type ->
        Path (["i64"], []) (* TODO: More integer types *)
    | Bool_type ->
        Path (["bool"], [])

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
        StructLit (name, fields)
    | Record_proj (head, label) ->
        (* TODO: get the type of the head *)
        (* TODO: lookup field mappings *)
        StructProj (
          compile_expr ctx head,
          Case_conv.quiet_snake_case label (* TODO: handle this better? *)
        )
    | List_lit exprs ->
        Vec_lit (List.map (compile_expr ctx) exprs)
    | Int_lit i -> I64Lit i
    | Bool_lit b -> Bool_lit b
    | Bool_elim (head, expr1, expr2) ->
        If_else (
          compile_expr ctx head,
          compile_stmts ctx expr1,
          compile_stmts ctx expr2
        )
    | Prim_app (prim, args) ->
        let prefix op args : Rust.expr =
          match args with
          | [x] -> Prefix_op (op, compile_expr ctx x)
          | _ -> failwith "invalid prim"
        and infix op args : Rust.expr =
          match args with
          | [x; y] -> Infix_op (compile_expr ctx x, op, compile_expr ctx y)
          | _ -> failwith "invalid prim"
        in
        match prim with
        (* See https://doc.rust-lang.org/reference/expressions/operator-expr.html
           for the semantics of Rust’s binary operators *)
        | Int_eq -> infix "==" args
        | Int_ne -> infix "!=" args
        | Int_le -> infix "<=" args
        | Int_lt -> infix "<" args
        | Int_gt -> infix ">" args
        | Int_ge -> infix ">=" args
        | Int_neg -> prefix "-" args
        | Int_add -> infix "+" args
        | Int_sub -> infix "-" args
        | Int_mul -> infix "*" args
        | Int_div -> infix "/" args
        | Int_logical_not -> prefix "!" args
        | Int_logical_and -> infix "&" args
        | Int_logical_or -> infix "|" args
        | Int_logical_xor -> infix "^" args
        | Int_logical_shl -> infix "<<" args
        | Int_arith_shr -> infix ">>" args
        | Int_logical_shr -> Path ["todo!(\"logical shift right\")"] (* FIXME *)

  let rec compile_format_stmts (ctx : context) (fmt : format) : Rust.stmts =
    match fmt with
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
    | Item_var name ->
        let name = compile_item_var ctx name in
        Call (Path [name], [Path ["input"]; Path ["pos"]])
    | Byte ->
        Call (Path ["read_byte"], [Path ["input"]; Path ["pos"]])
    | Repeat_len (len, elem_fmt) ->
        let elem_ty = Semantics.format_ty ctx.source_items fmt in
        Repeat_count (
          compile_expr ctx len,
          compile_format_expr ctx elem_fmt,
          Path (["Result"], [compile_ty ctx elem_ty; Placeholder])
        )
    (* Optimisation for let-bound formats *)
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

  let compile_program (items : program) : Rust.item list =
    let compile_items =
      ListLabels.fold_left
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
    (compile_items items).target_items |> List.rev

end