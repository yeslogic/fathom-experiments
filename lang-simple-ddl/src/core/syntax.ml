type index = int

module Label_map = Map.Make (String)

(** {1 Syntax} *)

type meta_id = int

type unsigned_int_constr = [
  | `UInt8
  | `UInt16
  | `UInt32
  | `UInt64
]

type signed_int_constr = [
  | `Int8
  | `Int16
  | `Int32
  | `Int64
]

type int_constr = [
  | unsigned_int_constr
  | signed_int_constr
]

type prim =
  | Eq of int_constr
  | Ne of int_constr
  | Le of int_constr
  | Lt of int_constr
  | Ge of int_constr
  | Gt of int_constr
  | Neg of signed_int_constr
  | Add of int_constr
  | Sub of int_constr
  | Mul of int_constr
  | Div of int_constr
  | Bit_not of int_constr
  | Bit_and of int_constr
  | Bit_or of int_constr
  | Bit_xor of int_constr
  | Bit_shl of int_constr
  | Bit_shr of int_constr

type ty =
  | Item_var of string
  | Meta_var of meta_id
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

let pp_constr ppf (prim : [< int_constr]) =
  match prim with
  | `UInt8 -> Format.pp_print_string ppf "uint64"
  | `UInt16 -> Format.pp_print_string ppf "uint64"
  | `UInt32 -> Format.pp_print_string ppf "uint64"
  | `UInt64 -> Format.pp_print_string ppf "uint64"
  | `Int8 -> Format.pp_print_string ppf "int64"
  | `Int16 -> Format.pp_print_string ppf "int64"
  | `Int32 -> Format.pp_print_string ppf "int64"
  | `Int64 -> Format.pp_print_string ppf "int64"

let pp_prim ppf (prim : prim) =
  match prim with
  | Eq constr -> Format.fprintf ppf "%a-eq" pp_constr constr
  | Ne constr -> Format.fprintf ppf "%a-ne" pp_constr constr
  | Le constr -> Format.fprintf ppf "%a-le" pp_constr constr
  | Lt constr -> Format.fprintf ppf "%a-lt" pp_constr constr
  | Ge constr -> Format.fprintf ppf "%a-ge" pp_constr constr
  | Gt constr -> Format.fprintf ppf "%a-gt" pp_constr constr
  | Neg constr -> Format.fprintf ppf "%a-neg" pp_constr constr
  | Add constr -> Format.fprintf ppf "%a-add" pp_constr constr
  | Sub constr -> Format.fprintf ppf "%a-sub" pp_constr constr
  | Mul constr -> Format.fprintf ppf "%a-mul" pp_constr constr
  | Div constr -> Format.fprintf ppf "%a-div" pp_constr constr
  | Bit_not constr -> Format.fprintf ppf "%a-bit-not" pp_constr constr
  | Bit_and constr -> Format.fprintf ppf "%a-bit-and" pp_constr constr
  | Bit_or constr -> Format.fprintf ppf "%a-bit-or" pp_constr constr
  | Bit_xor constr -> Format.fprintf ppf "%a-bit-xor" pp_constr constr
  | Bit_shl constr -> Format.fprintf ppf "%a-bit-shl" pp_constr constr
  | Bit_shr constr -> Format.fprintf ppf "%a-bit-shr" pp_constr constr

let rec pp_ty ppf (ty : ty) =
  match ty with
  | List_type elem_ty -> Format.fprintf ppf "@[<hv 2>#List %a@]" pp_atomic_ty elem_ty
  | ty -> pp_atomic_ty ppf ty

and pp_atomic_ty ppf ty =
  match ty with
  | Item_var name -> Format.pp_print_string ppf name
  | Meta_var id -> Format.fprintf ppf "$%i" id
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
