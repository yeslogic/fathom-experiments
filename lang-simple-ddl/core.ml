(** Core language *)

type index = int

module Label_map = Map.Make (String)

(** {1 Syntax} *)

type int_prim =
  | Eq
  | Ne
  | Le
  | Lt
  | Ge
  | Gt
  | Neg
  | Add
  | Sub
  | Mul
  | Div
  | Bit_not
  | Bit_and
  | Bit_or
  | Bit_xor
  | Bit_shl
  | Bit_shr

type prim =
  | UInt64 of int_prim
  | Int64 of int_prim

type ty =
  | Item_var of string
  | List_type of ty
  | UInt8_type
  | UInt16_type
  | UInt32_type
  | UInt64_type
  | Int8_type
  | Int16_type
  | Int32_type
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
  | UInt8_lit of Sized_numbers.uint8
  | UInt16_lit of Sized_numbers.uint16
  | UInt32_lit of Sized_numbers.uint32
  | UInt64_lit of Sized_numbers.uint64
  | Int8_lit of Sized_numbers.int8
  | Int16_lit of Sized_numbers.int16
  | Int32_lit of Sized_numbers.int32
  | Int64_lit of Sized_numbers.int64
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

let pp_int_prim ppf (prim : int_prim) =
  match prim with
  | Eq -> Format.pp_print_string ppf "eq"
  | Ne -> Format.pp_print_string ppf "ne"
  | Le -> Format.pp_print_string ppf "le"
  | Lt -> Format.pp_print_string ppf "lt"
  | Ge -> Format.pp_print_string ppf "ge"
  | Gt -> Format.pp_print_string ppf "gt"
  | Neg -> Format.pp_print_string ppf "neg"
  | Add -> Format.pp_print_string ppf "add"
  | Sub -> Format.pp_print_string ppf "sub"
  | Mul -> Format.pp_print_string ppf "mul"
  | Div -> Format.pp_print_string ppf "div"
  | Bit_not -> Format.pp_print_string ppf "bit-not"
  | Bit_and -> Format.pp_print_string ppf "bit-and"
  | Bit_or -> Format.pp_print_string ppf "bit-or"
  | Bit_xor -> Format.pp_print_string ppf "bit-xor"
  | Bit_shl -> Format.pp_print_string ppf "bit-shl"
  | Bit_shr -> Format.pp_print_string ppf "bit-shr"

let pp_prim ppf (prim : prim) =
  match prim with
  | UInt64 prim -> Format.fprintf ppf "uint64-%a" pp_int_prim prim
  | Int64 prim -> Format.fprintf ppf "int64-%a" pp_int_prim prim

let rec pp_ty ppf (ty : ty) =
  match ty with
  | List_type elem_ty -> Format.fprintf ppf "@[<hv 2>#List %a@]" pp_atomic_ty elem_ty
  | ty -> pp_atomic_ty ppf ty

and pp_atomic_ty ppf ty =
  match ty with
  | Item_var name -> Format.pp_print_string ppf name
  | UInt8_type -> Format.pp_print_string ppf "#UInt8"
  | UInt16_type -> Format.pp_print_string ppf "#UInt16"
  | UInt32_type -> Format.pp_print_string ppf "#UInt32"
  | UInt64_type -> Format.pp_print_string ppf "#UInt64"
  | Int8_type -> Format.pp_print_string ppf "#Int8"
  | Int16_type -> Format.pp_print_string ppf "#Int16"
  | Int32_type -> Format.pp_print_string ppf "#Int32"
  | Int64_type -> Format.pp_print_string ppf "#Int64"
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
  | UInt8_lit i -> Sized_numbers.UInt8.pp ppf i
  | UInt16_lit i -> Sized_numbers.UInt16.pp ppf i
  | UInt32_lit i -> Sized_numbers.UInt32.pp ppf i
  | UInt64_lit i -> Sized_numbers.UInt64.pp ppf i
  | Int8_lit i -> Sized_numbers.Int8.pp ppf i
  | Int16_lit i -> Sized_numbers.Int16.pp ppf i
  | Int32_lit i -> Sized_numbers.Int32.pp ppf i
  | Int64_lit i -> Sized_numbers.Int64.pp ppf i
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

  (** Neutral formats *)
  type nformat =
    | Item_var of string

  (** Type values *)
  type vty =
    | Unfold of nty * vty Lazy.t
    | Record_type of string * vty Label_map.t
    | List_type of vty
    | UInt8_type
    | UInt16_type
    | UInt32_type
    | UInt64_type
    | Int8_type
    | Int16_type
    | Int32_type
    | Int64_type
    | Bool_type
  (** Neutral types *)
  and nty =
    | Item_var of string
    | Format_repr of nformat

  (** Top-level items are “glued”, meaning that we remember the item they refer
      to. See. {{:https://andraskovacs.github.io/pdfs/wits24prez.pdf} “Efficient
      Elaboration with Controlled Definition Unfolding} and
      {{:https://github.com/AndrasKovacs/elaboration-zoo/blob/master/GluedEval.hs}
      GluedEval.hs} for more information on this technique.
  *)

  (** Expression values *)
  type vexpr =
    | UInt8_lit of Sized_numbers.uint8
    | UInt16_lit of Sized_numbers.uint16
    | UInt32_lit of Sized_numbers.uint32
    | UInt64_lit of Sized_numbers.uint64
    | Int8_lit of Sized_numbers.int8
    | Int16_lit of Sized_numbers.int16
    | Int32_lit of Sized_numbers.int32
    | Int64_lit of Sized_numbers.int64
    | Bool_lit of bool
    | List_lit of vexpr list
    | Record_lit of string * vexpr Label_map.t
    | TupleLit of vexpr list

  type env =
    vexpr list

  let rec force_vty (vty : vty) : vty =
    match vty with
    | Unfold (_, vty) -> (force_vty [@tailcall]) (Lazy.force vty)
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
        Unfold (Item_var name, Lazy.from_fun vty)
    | List_type elem_ty -> List_type (eval_ty items elem_ty)
    | UInt8_type -> UInt8_type
    | UInt16_type -> UInt16_type
    | UInt32_type -> UInt32_type
    | UInt64_type -> UInt64_type
    | Int8_type -> Int8_type
    | Int16_type -> Int16_type
    | Int32_type -> Int32_type
    | Int64_type -> Int64_type
    | Bool_type -> Bool_type
    | Format_repr (Item_var name) ->
        let vty () = eval_ty items (format_ty items (Item_var name)) in
        Unfold (Format_repr (Item_var name), Lazy.from_fun vty)
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
    let[@warning "-partial-match"] uint64_uint64_bool f [UInt64_lit x; UInt64_lit y] = Bool_lit (f x y) in
    let[@warning "-partial-match"] uint64_uint64 f [UInt64_lit x] = UInt64_lit (f x) in
    let[@warning "-partial-match"] uint64_uint64_uint64 f [UInt64_lit x; UInt64_lit y] = UInt64_lit (f x y) in
    let[@warning "-partial-match"] uint64_uint64'_uint64 f [UInt64_lit x; UInt64_lit y] = UInt64_lit (f x (Sized_numbers.UInt64.to_int_opt y |> Option.get)) in

    let[@warning "-partial-match"] int64_int64_bool f [Int64_lit x; Int64_lit y] = Bool_lit (f x y) in
    let[@warning "-partial-match"] int64_int64 f [Int64_lit x] = Int64_lit (f x) in
    let[@warning "-partial-match"] int64_int64_int64 f [Int64_lit x; Int64_lit y] = Int64_lit (f x y) in
    let[@warning "-partial-match"] int64_int64'_int64 f [Int64_lit x; Int64_lit y] = Int64_lit (f x (Sized_numbers.Int64.to_int_opt y |> Option.get)) in

    match prim with
    | UInt64 Eq -> uint64_uint64_bool Sized_numbers.UInt64.equal
    | UInt64 Ne -> uint64_uint64_bool Sized_numbers.UInt64.O.( <> )
    | UInt64 Le -> uint64_uint64_bool Sized_numbers.UInt64.O.( <= )
    | UInt64 Lt -> uint64_uint64_bool Sized_numbers.UInt64.O.( < )
    | UInt64 Gt -> uint64_uint64_bool Sized_numbers.UInt64.O.( < )
    | UInt64 Ge -> uint64_uint64_bool Sized_numbers.UInt64.O.( >= )
    | UInt64 Neg -> uint64_uint64 Sized_numbers.UInt64.neg
    | UInt64 Add -> uint64_uint64_uint64 Sized_numbers.UInt64.add
    | UInt64 Sub -> uint64_uint64_uint64 Sized_numbers.UInt64.sub
    | UInt64 Mul -> uint64_uint64_uint64 Sized_numbers.UInt64.mul
    | UInt64 Div -> uint64_uint64_uint64 Sized_numbers.UInt64.div
    | UInt64 Bit_not -> uint64_uint64 Sized_numbers.UInt64.lognot
    | UInt64 Bit_and -> uint64_uint64_uint64 Sized_numbers.UInt64.logand
    | UInt64 Bit_or -> uint64_uint64_uint64 Sized_numbers.UInt64.logor
    | UInt64 Bit_xor -> uint64_uint64_uint64 Sized_numbers.UInt64.logxor
    | UInt64 Bit_shl -> uint64_uint64'_uint64 Sized_numbers.UInt64.shift_left
    | UInt64 Bit_shr -> uint64_uint64'_uint64 Sized_numbers.UInt64.shift_right

    | Int64 Eq -> int64_int64_bool Sized_numbers.Int64.equal
    | Int64 Ne -> int64_int64_bool Sized_numbers.Int64.O.( <> )
    | Int64 Le -> int64_int64_bool Sized_numbers.Int64.O.( <= )
    | Int64 Lt -> int64_int64_bool Sized_numbers.Int64.O.( < )
    | Int64 Gt -> int64_int64_bool Sized_numbers.Int64.O.( < )
    | Int64 Ge -> int64_int64_bool Sized_numbers.Int64.O.( >= )
    | Int64 Neg -> int64_int64 Sized_numbers.Int64.neg
    | Int64 Add -> int64_int64_int64 Sized_numbers.Int64.add
    | Int64 Sub -> int64_int64_int64 Sized_numbers.Int64.sub
    | Int64 Mul -> int64_int64_int64 Sized_numbers.Int64.mul
    | Int64 Div -> int64_int64_int64 Sized_numbers.Int64.div
    | Int64 Bit_not -> int64_int64 Sized_numbers.Int64.lognot
    | Int64 Bit_and -> int64_int64_int64 Sized_numbers.Int64.logand
    | Int64 Bit_or -> int64_int64_int64 Sized_numbers.Int64.logor
    | Int64 Bit_xor -> int64_int64_int64 Sized_numbers.Int64.logxor
    | Int64 Bit_shl -> int64_int64'_int64 Sized_numbers.Int64.shift_left
    | Int64 Bit_shr -> int64_int64'_int64 Sized_numbers.Int64.shift_right

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
    | UInt8_lit i -> UInt8_lit i
    | UInt16_lit i -> UInt16_lit i
    | UInt32_lit i -> UInt32_lit i
    | UInt64_lit i -> UInt64_lit i
    | Int8_lit i -> Int8_lit i
    | Int16_lit i -> Int16_lit i
    | Int32_lit i -> Int32_lit i
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

  let rec quote_vty ~(unfold : bool) (vty : vty) : ty =
    match vty with
    | Unfold (_, vty) when unfold -> quote_vty ~unfold (Lazy.force vty)
    | Unfold (Item_var name, _) -> Item_var name
    | Unfold (Format_repr (Item_var name), _) -> Format_repr (Item_var name)
    | Record_type (name, _) -> Item_var name
    | List_type vty -> List_type (quote_vty ~unfold vty)
    | UInt8_type -> UInt8_type
    | UInt16_type -> UInt16_type
    | UInt32_type -> UInt32_type
    | UInt64_type -> UInt64_type
    | Int8_type -> Int8_type
    | Int16_type -> Int16_type
    | Int32_type -> Int32_type
    | Int64_type -> Int64_type
    | Bool_type -> Bool_type


  (** {1 Unification} *)

  exception Failed_to_unify

  let rec unify_vtys (items : program) (vty1 : vty) (vty2 : vty) =
    match force_vty vty1, force_vty vty2 with
    | Unfold (Item_var name1, _), Unfold (Item_var name2, _) when name1 = name2 -> ()
    | Unfold (Format_repr (Item_var name1), _), Unfold (Format_repr (Item_var name2), _) when name1 = name2 -> ()

    | Unfold (_, vty1), vty2 | vty2, Unfold (_, vty1) ->
        unify_vtys items (Lazy.force vty1) vty2

    | Record_type (name1, _), Record_type (name2, _) when name1 = name2 -> ()
    | List_type elem_vty1, List_type elem_vty2 -> unify_vtys items elem_vty1 elem_vty2
    | UInt8_type, UInt8_type -> ()
    | UInt16_type, UInt16_type -> ()
    | UInt32_type, UInt32_type -> ()
    | UInt64_type, UInt64_type -> ()
    | Int8_type, Int8_type -> ()
    | Int16_type, Int16_type -> ()
    | Int32_type, Int32_type -> ()
    | Int64_type, Int64_type -> ()
    | Bool_type, Bool_type -> ()
    | _ -> raise Failed_to_unify


  (** {2 Decode semantics} *)

  let rec decode_format (items : program) (locals : env) (fmt : format) : vexpr Decoder.t =
    match fmt with
    | Item_var name ->
        begin match List.assoc name items with
        | Format_def fmt -> decode_format items [] fmt
        | _ -> invalid_arg "not a format item"
        end
    | Byte ->
        Decoder.byte
        |> Decoder.map (fun x -> Int64_lit (Int64.of_int (int_of_char x)))
    | Repeat_len (len, elem_fmt) ->
        begin match eval_expr items locals len with
        | Int64_lit len ->
            Decoder.repeat_len len (decode_format items locals elem_fmt)
            |> Decoder.map (fun xs -> List_lit xs)
        | _ -> failwith "integer expected"
        end
    | Bind (_, def_fmt, body_fmt) ->
        Decoder.bind (decode_format items locals def_fmt)
          (fun def -> decode_format items (def :: locals) body_fmt)
    | Pure (_, expr) -> Decoder.pure (eval_expr items locals expr)
    | Fail _ -> Decoder.fail
    | Bool_elim (head, fmt1, fmt2) ->
        begin match eval_expr items locals head with
        | Bool_lit true -> decode_format items locals fmt1
        | Bool_lit false -> decode_format items locals fmt2
        | _ -> failwith "boolean expected"
        end

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
    | Item_var name -> Path ([compile_item_var ctx name], [])
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
    | UInt8_lit i -> U8Lit i
    | UInt16_lit i -> U16Lit i
    | UInt32_lit i -> U32Lit i
    | UInt64_lit i -> U64Lit i
    | Int8_lit i -> I8Lit i
    | Int16_lit i -> I16Lit i
    | Int32_lit i -> I32Lit i
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
        let int_prim_app (prim : int_prim) =
          match prim with
          | Eq -> infix "==" args
          | Ne -> infix "!=" args
          | Le -> infix "<=" args
          | Lt -> infix "<" args
          | Gt -> infix ">" args
          | Ge -> infix ">=" args
          | Neg -> prefix "-" args
          | Add -> infix "+" args
          | Sub -> infix "-" args
          | Mul -> infix "*" args
          | Div -> infix "/" args
          | Bit_not -> prefix "!" args
          | Bit_and -> infix "&" args
          | Bit_or -> infix "|" args
          | Bit_xor -> infix "^" args
          | Bit_shl -> infix "<<" args
          | Bit_shr -> infix ">>" args
        in
        match prim with
        | UInt64 prim -> int_prim_app prim
        | Int64 prim -> int_prim_app prim

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
