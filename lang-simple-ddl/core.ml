(** Core language *)

type index = int

module Label_map = Map.Make (String)

(** {1 Syntax} *)

type prim =
  | Int64_eq
  | Int64_ne
  | Int64_le
  | Int64_lt
  | Int64_ge
  | Int64_gt
  | Int64_neg
  | Int64_add
  | Int64_sub
  | Int64_mul
  | Int64_div
  | Int64_logical_not
  | Int64_logical_and
  | Int64_logical_or
  | Int64_logical_xor
  | Int64_logical_shl
  | Int64_arith_shr
  | Int64_logical_shr

type ty =
  | Item_var of string
  | List_type of ty
  | Int64_type
  | Bool_type
  | Format_repr of format

and expr =
  | Item_var of string
  | Local_var of index
  | Let of string * ty * expr * expr
  | Record_lit of string * expr Label_map.t
  | Record_proj of expr * string
  | List_lit of expr list
  | Int64_lit of int64
  | Bool_lit of bool
  | Bool_elim of expr * expr * expr
  | Prim_app of prim * expr list

and format =
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

let pp_prim ppf (prim : prim) =
  match prim with
  | Int64_eq -> Format.pp_print_string ppf "int64-eq"
  | Int64_ne -> Format.pp_print_string ppf "int64-ne"
  | Int64_le -> Format.pp_print_string ppf "int64-le"
  | Int64_lt -> Format.pp_print_string ppf "int64-lt"
  | Int64_ge -> Format.pp_print_string ppf "int64-ge"
  | Int64_gt -> Format.pp_print_string ppf "int64-gt"
  | Int64_neg -> Format.pp_print_string ppf "int64-neg"
  | Int64_add -> Format.pp_print_string ppf "int64-add"
  | Int64_sub -> Format.pp_print_string ppf "int64-sub"
  | Int64_mul -> Format.pp_print_string ppf "int64-mul"
  | Int64_div -> Format.pp_print_string ppf "int64-div"
  | Int64_logical_not -> Format.pp_print_string ppf "int64-logical-not"
  | Int64_logical_and -> Format.pp_print_string ppf "int64-logical-and"
  | Int64_logical_or -> Format.pp_print_string ppf "int64-logical-or"
  | Int64_logical_xor -> Format.pp_print_string ppf "int64-logical-xor"
  | Int64_logical_shl -> Format.pp_print_string ppf "int64-logical-shl"
  | Int64_arith_shr -> Format.pp_print_string ppf "int64-arith-shr"
  | Int64_logical_shr -> Format.pp_print_string ppf "int64-logical-shr"

let rec pp_ty ppf (ty : ty) =
  match ty with
  | List_type elem_ty -> Format.fprintf ppf "@[<hv 2>#List %a@]" pp_atomic_ty elem_ty
  | ty -> pp_atomic_ty ppf ty

and pp_atomic_ty ppf ty =
  match ty with
  | Item_var name -> Format.pp_print_string ppf name
  | Int64_type -> Format.pp_print_string ppf "#Int"
  | Bool_type -> Format.pp_print_string ppf "#Bool"
  | Format_repr fmt -> Format.fprintf ppf "%a.Repr" (pp_atomic_format []) fmt
  | ty -> Format.fprintf ppf "@[(%a)@]" pp_ty ty

and pp_name_ann ppf (name, ty) =
  Format.fprintf ppf "@[<2>@[%s :@]@ %a@]" name pp_ty ty

and pp_expr names ppf (expr : expr) =
  match expr with
  | Let _ as expr ->
      let rec go names ppf expr =
        match expr with
        | Let (name, def_ty, def, body) ->
            Format.fprintf ppf "@[<2>@[let %a@ :=@]@ @[%a;@]@]@ %a"
              pp_name_ann (name, def_ty)
              (pp_expr names) def
              (go (name :: names)) body
        | expr -> Format.fprintf ppf "@[%a@]" (pp_expr names) expr
      in
      Format.fprintf ppf "@[<v>%a@]" (go names) expr
  | Bool_elim (head, expr1, expr2 ) ->
      Format.fprintf ppf "@[<hv>@[if@ %a@ then@]@;<1 2>@[%a@]@ else@;<1 2>@[%a@]@]"
        (pp_app_expr names) head
        (pp_app_expr names) expr1
        (pp_expr names) expr2
  | expr -> pp_app_expr names ppf expr

and pp_app_expr names ppf expr =
  match expr with
  | Record_lit (name, fields) when Label_map.is_empty fields ->
      Format.fprintf ppf "@[<hv>@[%s@ {}@]" name
  | Record_lit (name, fields) ->
      let pp_sep ppf () = Format.fprintf ppf ";" in
      let pp_field ppf (label, expr) =
        Format.fprintf ppf "@;<1 2>@[@[%s@ :=@]@ %a@]" label (pp_expr names) expr
      in
      Format.fprintf ppf "@[<hv>@[%s@ {@]%a@ }@]"
        name
        (Format.pp_print_seq ~pp_sep pp_field)
        (Label_map.to_seq fields)

  | Prim_app (prim, args) ->
      let pp_sep fmt () = Format.fprintf fmt "@ " in
      Format.fprintf ppf "@[<hv 2>#%a@ %a@]"
        pp_prim prim
        (Format.pp_print_list ~pp_sep (pp_print_proj_expr names)) args

  | expr -> pp_print_proj_expr names ppf expr

and pp_print_proj_expr names ppf expr =
  match expr with
  | Record_proj (head, label) ->
      Format.fprintf ppf "%a.%s" (pp_print_proj_expr names) head label
  | expr -> pp_atomic_expr names ppf expr

and pp_atomic_expr names ppf expr =
  match expr with
  | Item_var name -> Format.pp_print_string ppf name
  | Local_var index -> Format.pp_print_string ppf (List.nth names index)
  | List_lit elems ->
      let pp_sep fmt () = Format.fprintf fmt ",@ " in
      Format.fprintf ppf "@[[%a]@]"
        (Format.pp_print_list ~pp_sep (pp_print_proj_expr names)) elems
  | Int64_lit i -> Format.pp_print_string ppf (Int64.to_string i)
  | Bool_lit true -> Format.pp_print_string ppf "true"
  | Bool_lit false -> Format.pp_print_string ppf "false"
  | expr -> Format.fprintf ppf "@[(%a)@]" (pp_expr names) expr

and pp_format names ppf (fmt : format) =
  match fmt with
  | Pure (ty, expr) ->
      Format.fprintf ppf "@[<hv 2>#pure@ %a@ %a@]"
        pp_atomic_ty ty
        (pp_atomic_expr names) expr

  | Repeat_len (len, elem_fmt) ->
      Format.fprintf ppf "@[<hv 2>#repeat-len@ %a@ %a@]"
        (pp_atomic_expr names) len
        (pp_atomic_format names) elem_fmt

  | Bind _ as fmt ->
      let rec go names ppf expr =
        match expr with
        | Bind (name, def_fmt, body_fmt) ->
            Format.fprintf ppf "@[<2>@[let %s@ <-@]@ @[%a;@]@]@ %a"
              name
              (pp_format names) def_fmt
              (go (name :: names)) body_fmt
        | fmt -> Format.fprintf ppf "@[%a@]" (pp_format names) fmt
      in
      Format.fprintf ppf "@[<v>%a@]" (go names) fmt

  | Fail ty ->
      Format.fprintf ppf "@[<hv 2>#fail@ %a@]"
        pp_atomic_ty ty

  | Bool_elim (head, fmt1, fmt2) ->
      Format.fprintf ppf "@[<hv>@[if@ %a@ then@]@;<1 2>@[%a@]@ else@;<1 2>@[%a@]@]"
        (pp_app_expr names) head
        (pp_atomic_format names) fmt1
        (pp_format names) fmt2

  | fmt -> pp_atomic_format names ppf fmt

and pp_atomic_format names ppf (fmt : format) =
  match fmt with
  | Item_var name -> Format.pp_print_string ppf name
  | Byte -> Format.pp_print_string ppf "byte"
  | fmt -> Format.fprintf ppf "@[(%a)@]" (pp_format names) fmt

let pp_indent f ppf x =
  Format.fprintf ppf "@;<1 2>%a" f x

let pp_indent_vbox f ppf x =
  Format.fprintf ppf "@;<1 2>@[<v>%a@]" f x

let pp_item (ppf : Format.formatter) (item :  string * item) =
  match item with
  | name, Type_def ty ->
      Format.fprintf ppf "@[<hv 2>@[type@ %s@ :=@]@ %a;@]@."
        name
        pp_ty ty
  | name, Record_type fields ->
      let pp_field ppf (label, ty) =
        Format.fprintf ppf "@[@[%s@ :@]@ %a;@]" label pp_ty ty
      in
      Format.fprintf ppf "@[<v>@[type@ %s@ {@]%a@ }@]@."
        name
        (pp_indent_vbox (Format.pp_print_seq pp_field))
        (Label_map.to_seq fields)
  | name, Format_def ty ->
      Format.fprintf ppf "@[<hv 2>@[format@ %s@ :=@]@ %a;@]@."
        name
        (pp_format []) ty
  | name, Expr_def (ty, expr) ->
      Format.fprintf ppf "@[<hv 2>@[def@ %a@ :=@]@ %a;@]@."
        pp_name_ann (name, ty)
        (pp_expr []) expr

let pp_program (ppf : Format.formatter) (items : program) =
  Format.pp_print_list pp_item ppf items



module Semantics = struct

  (** {1 Semantic domain} *)

  type vty =
    | Item_var of string * vty Lazy.t
    | Record_type of string * vty Label_map.t
    | List_type of vty
    | Int64_type
    | Bool_type

  (** Top-level items are “glued”, meaning that we remember the item they refer to. See.
      {{:https://github.com/AndrasKovacs/elaboration-zoo/blob/master/GluedEval.hs} GluedEval.hs}
      for more information on this technique.
  *)

  type vexpr =
    | Int64_lit of int64
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
    | Int64_type -> Int64_type
    | Bool_type -> Bool_type
    | Format_repr fmt ->
        eval_ty items (format_ty items fmt)

  and format_ty (items : program) (fmt : format) : ty =
    match fmt with
    | Item_var name ->
        begin match List.assoc name items with
        | Format_def fmt -> format_ty items fmt
        | _ -> invalid_arg "expected format"
        end
    | Byte -> Int64_type
    | Repeat_len (_, elem_fmt) -> List_type (format_ty items elem_fmt)
    | Bind (_, _, body_fmt) -> format_ty items body_fmt
    | Pure (ty, _) -> ty
    | Fail ty -> ty
    | Bool_elim (_, fmt1, _) -> format_ty items fmt1

  let prim_app (prim : prim) : vexpr list -> vexpr =
    match prim with
    | Int64_eq -> fun[@warning "-partial-match"] [Int64_lit x; Int64_lit y] -> Bool_lit (Int64.equal x y)
    | Int64_ne -> fun[@warning "-partial-match"] [Int64_lit x; Int64_lit y] -> Bool_lit (not (Int64.equal x y))
    | Int64_le -> fun[@warning "-partial-match"] [Int64_lit x; Int64_lit y] -> Bool_lit (x <= y)
    | Int64_lt -> fun[@warning "-partial-match"] [Int64_lit x; Int64_lit y] -> Bool_lit (x < y)
    | Int64_gt -> fun[@warning "-partial-match"] [Int64_lit x; Int64_lit y] -> Bool_lit (x < y)
    | Int64_ge -> fun[@warning "-partial-match"] [Int64_lit x; Int64_lit y] -> Bool_lit (x >= y)
    | Int64_neg -> fun[@warning "-partial-match"] [Int64_lit x] -> Int64_lit (Int64.neg x)
    | Int64_add -> fun[@warning "-partial-match"] [Int64_lit x; Int64_lit y] -> Int64_lit (Int64.add x y)
    | Int64_sub -> fun[@warning "-partial-match"] [Int64_lit x; Int64_lit y] -> Int64_lit (Int64.sub x y)
    | Int64_mul -> fun[@warning "-partial-match"] [Int64_lit x; Int64_lit y] -> Int64_lit (Int64.mul x y)
    | Int64_div -> fun[@warning "-partial-match"] [Int64_lit x; Int64_lit y] -> Int64_lit (Int64.div x y)
    | Int64_logical_not -> fun[@warning "-partial-match"] [Int64_lit x] -> Int64_lit (Int64.lognot x)
    | Int64_logical_and -> fun[@warning "-partial-match"] [Int64_lit x; Int64_lit y] -> Int64_lit (Int64.logand x y)
    | Int64_logical_or -> fun[@warning "-partial-match"] [Int64_lit x; Int64_lit y] -> Int64_lit (Int64.logor x y)
    | Int64_logical_xor -> fun[@warning "-partial-match"] [Int64_lit x; Int64_lit y] -> Int64_lit (Int64.logxor x y)
    | Int64_logical_shl -> fun[@warning "-partial-match"] [Int64_lit x; Int64_lit y] -> Int64_lit Int64.(shift_left x (to_int y))
    | Int64_arith_shr -> fun[@warning "-partial-match"] [Int64_lit x; Int64_lit y] -> Int64_lit Int64.(shift_right x (to_int y))
    | Int64_logical_shr -> fun[@warning "-partial-match"] [Int64_lit x; Int64_lit y] -> Int64_lit Int64.(shift_right_logical x (to_int y))

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
    | Int64_lit i -> Int64_lit i
    | Bool_lit b -> Bool_lit b
    | Bool_elim (head, expr1, expr2) ->
        begin match eval_expr items locals head with
        | Bool_lit true -> eval_expr items locals expr1
        | Bool_lit false -> eval_expr items locals expr2
        | _ -> failwith "boolean expected"
        end
    | Prim_app (prim, args) ->
        prim_app prim (List.map (eval_expr items locals) args)


  (** {1 Quotation} *)

  let rec quote_vty ?(unfold_items=false) (vty : vty) : ty =
    match vty with
    | Item_var (_, vty) when unfold_items -> quote_vty ~unfold_items (Lazy.force vty)
    | Item_var (name, _) -> Item_var name
    | Record_type (name, _) -> Item_var name
    | List_type vty -> List_type (quote_vty ~unfold_items vty)
    | Int64_type -> Int64_type
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
    | Int64_type, Int64_type -> ()
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
            | Int64_lit len ->
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
          pos + 1, Int64_lit (Int64.of_int (int_of_char (Bytes.unsafe_get input pos)))
        else
          raise (Decode_failure pos)

    and decode_elems (locals : env) (len : int64) (elem_fmt : format) : vexpr list decoder =
      fun ~input ~pos ->
        if Int64.zero = len then
          pos, []
        else
          let pos, vexpr = decode_format locals elem_fmt ~input ~pos in
          let pos, vexprs = decode_elems locals (Int64.pred len) elem_fmt ~input ~pos in
          pos, vexpr :: vexprs
    in

    decode_format [] fmt

end

module Compile = struct

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
    | Item_var name ->
        Path ([compile_item_var ctx name], [])
    | List_type ty ->
        Path (["Vec"], [compile_ty ctx ty])
    | Int64_type ->
        Path (["i64"], []) (* TODO: More integer types *)
    | Bool_type ->
        Path (["bool"], [])
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
    | Int64_lit i -> I64Lit i
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
        | Int64_eq -> infix "==" args
        | Int64_ne -> infix "!=" args
        | Int64_le -> infix "<=" args
        | Int64_lt -> infix "<" args
        | Int64_gt -> infix ">" args
        | Int64_ge -> infix ">=" args
        | Int64_neg -> prefix "-" args
        | Int64_add -> infix "+" args
        | Int64_sub -> infix "-" args
        | Int64_mul -> infix "*" args
        | Int64_div -> infix "/" args
        | Int64_logical_not -> prefix "!" args
        | Int64_logical_and -> infix "&" args
        | Int64_logical_or -> infix "|" args
        | Int64_logical_xor -> infix "^" args
        | Int64_logical_shl -> infix "<<" args
        | Int64_arith_shr -> infix ">>" args
        | Int64_logical_shr -> Path ["todo!(\"logical shift right\")"] (* FIXME *)

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

end
