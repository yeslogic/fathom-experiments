type ty =
  | Placeholder
  | Path of string list * ty list
  | Ref of ty
  | RefMut of ty
  | Slice of ty
  | Unit

type expr =
  | Path of string list
  | Call of expr * expr list
  | PrefixOp of string * expr
  | PostfixOp of expr * string
  | InfixOp of expr * string * expr
  | VecLit of expr list
  | I64Lit of int
  | BoolLit of bool
  | UnitLit
  | StructLit of string * (string * expr) list
  | StructProj of expr * string
  | IfElse of expr * stmts * stmts
  | Block of stmts
  | RepeatCount of expr * expr * ty

and stmt =
  | Let of string * ty option * expr

and stmts =
  stmt list * expr option

type item =
  | Type of string * ty
  | Struct of string * (string * ty) list
  | Const of string * ty * expr
  | Fn of string * (string * ty) list * ty * stmts


(** {1 Pretty printing} *)

(** The goal is not to replicate the output of rustfmt, but follow standard Rust
    conventions closely and be reasonably easy to audit. *)

let pp_indent f ppf x =
  Format.fprintf ppf "@;<1 4>%a" f x

let pp_indent_vbox f ppf x =
  Format.fprintf ppf "@;<1 4>@[<v>%a@]" f x

let pp_path (ppf : Format.formatter) (parts : string list) =
  let pp_sep ppf () = Format.fprintf ppf "::" in
  Format.pp_print_list Format.pp_print_string ~pp_sep ppf parts

let rec pp_ty (ppf : Format.formatter) (ty : ty) =
  match ty with
  | Placeholder ->
      Format.fprintf ppf "_"
  | Path (parts, []) ->
      Format.fprintf ppf "%a"
        pp_path parts
  | Path (parts, ty_args) ->
      let pp_sep ppf () = Format.fprintf ppf ",@ " in
      Format.fprintf ppf "@[<h>%a<%a>@]"
        pp_path parts
        (Format.pp_print_list pp_ty ~pp_sep) ty_args
    | Ref ty ->
        Format.fprintf ppf "&%a"
          pp_ty ty
    | RefMut ty ->
        Format.fprintf ppf "@[&mut@ %a@]"
          pp_ty ty
    | Slice ty ->
        Format.fprintf ppf "[%a]"
          pp_ty ty
    | Unit ->
        Format.fprintf ppf "()"

let rec pp_expr (ppf : Format.formatter) (expr : expr) =
  match expr with
  | PrefixOp (op, expr) ->
      Format.fprintf ppf "%s%a"
        op
        pp_atomic_expr expr
  | PostfixOp (expr, op) ->
      Format.fprintf ppf "%a%s"
        pp_atomic_expr expr
        op
  | InfixOp (_, op, _) ->
      let rec go ppf expr =
        match expr with
        (* Print the same operator at same precedence *)
        | InfixOp (expr1, op', expr2) when op = op' ->
            Format.fprintf ppf "%a@ %s@ %a"
              go expr1
              op
              go expr2
        (* Otherwise use parentheses *)
        | expr ->
            Format.fprintf ppf "%a"
              pp_atomic_expr expr
      in
      Format.fprintf ppf "@[%a@]"
        go expr
  | IfElse (expr, stmts1, stmts2) ->
      Format.fprintf ppf "@[<hv>@[if@ %a@ {@]%a@ @[}@ else@ {@]%a@ }@]"
        pp_expr expr
        (pp_indent_vbox pp_stmts) stmts1
        (pp_indent_vbox pp_stmts) stmts2
  | expr ->
      pp_atomic_expr ppf expr

and pp_atomic_expr (ppf : Format.formatter) (expr : expr) =
  match expr with
  | Path parts ->
      pp_path ppf parts
  | Call (expr, args) ->
      let pp_sep ppf () = Format.fprintf ppf ",@ " in
      Format.fprintf ppf "@[%a(%a)@]"
        pp_expr expr
        (Format.pp_print_list pp_expr ~pp_sep) args
  | VecLit exprs ->
      let pp_sep ppf () = Format.fprintf ppf ",@ " in
      Format.fprintf ppf "vec![%a]"
        (Format.pp_print_list pp_expr ~pp_sep) exprs
  | I64Lit i ->
      Format.pp_print_int ppf i
  | BoolLit true ->
      Format.fprintf ppf "true"
  | BoolLit false ->
      Format.fprintf ppf "false"
  | UnitLit->
      Format.fprintf ppf "()"
  | StructLit (name, fields) ->
      let pp_sep ppf () = Format.fprintf ppf "," in
      let pp_field ppf (label, expr) =
        (* Render field with punning if possible *)
        match expr with
        | Path ([x]) when x = label -> Format.fprintf ppf "%s" x
        | _ -> Format.fprintf ppf "@[%s:@ %a@]" label pp_expr expr
      in
      Format.fprintf ppf "@[<hv>@[%s@ {@]%a@ }@]"
        name
        (Format.pp_print_list (pp_indent pp_field) ~pp_sep) fields
        (* TODO: trailing comma with [pp_print_if_newline] *)
  | StructProj (expr, label) ->
      Format.fprintf ppf "%a.%s"
        pp_expr expr
        label
  | Block stmts ->
      Format.fprintf ppf "@[<hv>{%a@ }@]"
        (pp_indent_vbox pp_stmts) stmts
  | RepeatCount (count_expr, elem_expr, ty) ->
      Format.fprintf ppf "@[<hv 4>@[(0..%a)@]@[.map(|_| %a)@]@[.collect::<%a>()@]@]"
        pp_expr count_expr
        pp_expr elem_expr
        pp_ty ty
  | expr ->
      Format.fprintf ppf "(%a)"
        pp_expr expr

and pp_stmt (ppf : Format.formatter) (stmt : stmt) =
  match stmt with
  | Let (name, None, expr) ->
      Format.fprintf ppf "@[<hv 4>@[let@ %s@ =@]@ @[%a;@]@]"
        name
        pp_expr expr
  | Let (name, Some ty, expr) ->
      Format.fprintf ppf "@[<hv 4>@[let@ %s:@ %a@ =@]@ @[%a;@]@]"
        name
        pp_ty ty
        pp_expr expr

and pp_stmts (ppf : Format.formatter) (stmts, expr : stmts) =
  match stmts with
  | [] -> Format.pp_print_option pp_expr ppf expr
  | stmt :: stmts ->
      Format.fprintf ppf "%a@ %a"
        pp_stmt stmt
        pp_stmts (stmts, expr)

let pp_item (ppf : Format.formatter) (item : item) =
  match item with
  | Type (name, ty) ->
      Format.fprintf ppf "type@ %s@ =@ %a;@."
        name
        pp_ty ty
  | Struct (name, fields) ->
      let pp_field ppf (label, ty) =
        Format.fprintf ppf "@[%s:@ %a,@]" label pp_ty ty
      in
      Format.fprintf ppf "@[<v>@[struct@ %s@ {@]%a@ }@]@."
        name
        (pp_indent_vbox (Format.pp_print_list pp_field)) fields
  | Const (name, ty, expr) ->
      Format.fprintf ppf "@[<4>@[const@ %s:@ %a@ =@]@ %a;@]@."
        name
        pp_ty ty
        pp_expr expr
  | Fn (name, params, ret_ty, body) ->
      let pp_sep ppf () = Format.fprintf ppf ",@ " in
      let pp_param ppf (name, ty) =
        Format.fprintf ppf "@[%s:@ %a@]" name pp_ty ty
      in
      Format.fprintf ppf "@[<v>@[fn %s(%a) -> %a {@]%a@ }@]@."
        name
        (Format.pp_print_list pp_param ~pp_sep) params
        pp_ty ret_ty
        (pp_indent_vbox pp_stmts) body

let pp_program (ppf : Format.formatter) (items : item list) =
  Format.fprintf ppf "fn read_byte(input: &[u8], pos: &mut usize) -> Result<i64, ()> {@.";
  Format.fprintf ppf "    let byte = input.get(*pos).ok_or(())?;@.";
  Format.fprintf ppf "    *pos +=1;@.";
  Format.fprintf ppf "    Ok(i64::from(*byte))@.";
  Format.fprintf ppf "}@.";
  Format.fprintf ppf "@.";
  Format.pp_print_list pp_item ppf items
