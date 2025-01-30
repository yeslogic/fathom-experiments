open Syntax

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
  let open Sized_numbers in

  (* TODO: Clean this up!! *)

  match prim with
  | Eq `UInt8 -> fun[@warning"-partial-match"] [UInt8_lit x; UInt8_lit y] -> Bool_lit (UInt8.equal x y)
  | Ne `UInt8 -> fun[@warning"-partial-match"] [UInt8_lit x; UInt8_lit y] -> Bool_lit (UInt8.O.(x <> y))
  | Le `UInt8 -> fun[@warning"-partial-match"] [UInt8_lit x; UInt8_lit y] -> Bool_lit (UInt8.O.(x <= y))
  | Lt `UInt8 -> fun[@warning"-partial-match"] [UInt8_lit x; UInt8_lit y] -> Bool_lit (UInt8.O.(x < y))
  | Gt `UInt8 -> fun[@warning"-partial-match"] [UInt8_lit x; UInt8_lit y] -> Bool_lit (UInt8.O.(x < y))
  | Ge `UInt8 -> fun[@warning"-partial-match"] [UInt8_lit x; UInt8_lit y] -> Bool_lit (UInt8.O.(x >= y))
  | Add `UInt8 -> fun[@warning"-partial-match"] [UInt8_lit x; UInt8_lit y] -> UInt8_lit (UInt8.add x y)
  | Sub `UInt8 -> fun[@warning"-partial-match"] [UInt8_lit x; UInt8_lit y] -> UInt8_lit (UInt8.sub x y)
  | Mul `UInt8 -> fun[@warning"-partial-match"] [UInt8_lit x; UInt8_lit y] -> UInt8_lit (UInt8.mul x y)
  | Div `UInt8 -> fun[@warning"-partial-match"] [UInt8_lit x; UInt8_lit y] -> UInt8_lit (UInt8.div x y)
  | Bit_not `UInt8 -> fun[@warning"-partial-match"] [UInt8_lit x] -> UInt8_lit (UInt8.lognot x)
  | Bit_and `UInt8 -> fun[@warning"-partial-match"] [UInt8_lit x; UInt8_lit y] -> UInt8_lit (UInt8.logand x y)
  | Bit_or `UInt8 -> fun[@warning"-partial-match"] [UInt8_lit x; UInt8_lit y] -> UInt8_lit (UInt8.logor x y)
  | Bit_xor `UInt8 -> fun[@warning"-partial-match"] [UInt8_lit x; UInt8_lit y] -> UInt8_lit (UInt8.logxor x y)
  | Bit_shl `UInt8 -> fun[@warning"-partial-match"] [UInt8_lit x; UInt8_lit y] -> UInt8_lit (UInt8.(shift_left x (to_int y)))
  | Bit_shr `UInt8 -> fun[@warning"-partial-match"] [UInt8_lit x; UInt8_lit y] -> UInt8_lit (UInt8.(shift_right x (to_int y)))

  | Eq `UInt16 -> fun[@warning"-partial-match"] [UInt16_lit x; UInt16_lit y] -> Bool_lit (UInt16.equal x y)
  | Ne `UInt16 -> fun[@warning"-partial-match"] [UInt16_lit x; UInt16_lit y] -> Bool_lit (UInt16.O.(x <> y))
  | Le `UInt16 -> fun[@warning"-partial-match"] [UInt16_lit x; UInt16_lit y] -> Bool_lit (UInt16.O.(x <= y))
  | Lt `UInt16 -> fun[@warning"-partial-match"] [UInt16_lit x; UInt16_lit y] -> Bool_lit (UInt16.O.(x < y))
  | Gt `UInt16 -> fun[@warning"-partial-match"] [UInt16_lit x; UInt16_lit y] -> Bool_lit (UInt16.O.(x < y))
  | Ge `UInt16 -> fun[@warning"-partial-match"] [UInt16_lit x; UInt16_lit y] -> Bool_lit (UInt16.O.(x >= y))
  | Add `UInt16 -> fun[@warning"-partial-match"] [UInt16_lit x; UInt16_lit y] -> UInt16_lit (UInt16.add x y)
  | Sub `UInt16 -> fun[@warning"-partial-match"] [UInt16_lit x; UInt16_lit y] -> UInt16_lit (UInt16.sub x y)
  | Mul `UInt16 -> fun[@warning"-partial-match"] [UInt16_lit x; UInt16_lit y] -> UInt16_lit (UInt16.mul x y)
  | Div `UInt16 -> fun[@warning"-partial-match"] [UInt16_lit x; UInt16_lit y] -> UInt16_lit (UInt16.div x y)
  | Bit_not `UInt16 -> fun[@warning"-partial-match"] [UInt16_lit x] -> UInt16_lit (UInt16.lognot x)
  | Bit_and `UInt16 -> fun[@warning"-partial-match"] [UInt16_lit x; UInt16_lit y] -> UInt16_lit (UInt16.logand x y)
  | Bit_or `UInt16 -> fun[@warning"-partial-match"] [UInt16_lit x; UInt16_lit y] -> UInt16_lit (UInt16.logor x y)
  | Bit_xor `UInt16 -> fun[@warning"-partial-match"] [UInt16_lit x; UInt16_lit y] -> UInt16_lit (UInt16.logxor x y)
  | Bit_shl `UInt16 -> fun[@warning"-partial-match"] [UInt16_lit x; UInt16_lit y] -> UInt16_lit (UInt16.(shift_left x (to_int y)))
  | Bit_shr `UInt16 -> fun[@warning"-partial-match"] [UInt16_lit x; UInt16_lit y] -> UInt16_lit (UInt16.(shift_right x (to_int y)))

  | Eq `UInt32 -> fun[@warning"-partial-match"] [UInt32_lit x; UInt32_lit y] -> Bool_lit (UInt32.equal x y)
  | Ne `UInt32 -> fun[@warning"-partial-match"] [UInt32_lit x; UInt32_lit y] -> Bool_lit (UInt32.O.(x <> y))
  | Le `UInt32 -> fun[@warning"-partial-match"] [UInt32_lit x; UInt32_lit y] -> Bool_lit (UInt32.O.(x <= y))
  | Lt `UInt32 -> fun[@warning"-partial-match"] [UInt32_lit x; UInt32_lit y] -> Bool_lit (UInt32.O.(x < y))
  | Gt `UInt32 -> fun[@warning"-partial-match"] [UInt32_lit x; UInt32_lit y] -> Bool_lit (UInt32.O.(x < y))
  | Ge `UInt32 -> fun[@warning"-partial-match"] [UInt32_lit x; UInt32_lit y] -> Bool_lit (UInt32.O.(x >= y))
  | Add `UInt32 -> fun[@warning"-partial-match"] [UInt32_lit x; UInt32_lit y] -> UInt32_lit (UInt32.add x y)
  | Sub `UInt32 -> fun[@warning"-partial-match"] [UInt32_lit x; UInt32_lit y] -> UInt32_lit (UInt32.sub x y)
  | Mul `UInt32 -> fun[@warning"-partial-match"] [UInt32_lit x; UInt32_lit y] -> UInt32_lit (UInt32.mul x y)
  | Div `UInt32 -> fun[@warning"-partial-match"] [UInt32_lit x; UInt32_lit y] -> UInt32_lit (UInt32.div x y)
  | Bit_not `UInt32 -> fun[@warning"-partial-match"] [UInt32_lit x] -> UInt32_lit (UInt32.lognot x)
  | Bit_and `UInt32 -> fun[@warning"-partial-match"] [UInt32_lit x; UInt32_lit y] -> UInt32_lit (UInt32.logand x y)
  | Bit_or `UInt32 -> fun[@warning"-partial-match"] [UInt32_lit x; UInt32_lit y] -> UInt32_lit (UInt32.logor x y)
  | Bit_xor `UInt32 -> fun[@warning"-partial-match"] [UInt32_lit x; UInt32_lit y] -> UInt32_lit (UInt32.logxor x y)
  | Bit_shl `UInt32 -> fun[@warning"-partial-match"] [UInt32_lit x; UInt32_lit y] -> UInt32_lit (UInt32.(shift_left x (to_int_opt y |> Option.get)))
  | Bit_shr `UInt32 -> fun[@warning"-partial-match"] [UInt32_lit x; UInt32_lit y] -> UInt32_lit (UInt32.(shift_right x (to_int_opt y |> Option.get)))

  | Eq `UInt64 -> fun[@warning"-partial-match"] [UInt64_lit x; UInt64_lit y] -> Bool_lit (UInt64.equal x y)
  | Ne `UInt64 -> fun[@warning"-partial-match"] [UInt64_lit x; UInt64_lit y] -> Bool_lit (UInt64.O.(x <> y))
  | Le `UInt64 -> fun[@warning"-partial-match"] [UInt64_lit x; UInt64_lit y] -> Bool_lit (UInt64.O.(x <= y))
  | Lt `UInt64 -> fun[@warning"-partial-match"] [UInt64_lit x; UInt64_lit y] -> Bool_lit (UInt64.O.(x < y))
  | Gt `UInt64 -> fun[@warning"-partial-match"] [UInt64_lit x; UInt64_lit y] -> Bool_lit (UInt64.O.(x < y))
  | Ge `UInt64 -> fun[@warning"-partial-match"] [UInt64_lit x; UInt64_lit y] -> Bool_lit (UInt64.O.(x >= y))
  | Add `UInt64 -> fun[@warning"-partial-match"] [UInt64_lit x; UInt64_lit y] -> UInt64_lit (UInt64.add x y)
  | Sub `UInt64 -> fun[@warning"-partial-match"] [UInt64_lit x; UInt64_lit y] -> UInt64_lit (UInt64.sub x y)
  | Mul `UInt64 -> fun[@warning"-partial-match"] [UInt64_lit x; UInt64_lit y] -> UInt64_lit (UInt64.mul x y)
  | Div `UInt64 -> fun[@warning"-partial-match"] [UInt64_lit x; UInt64_lit y] -> UInt64_lit (UInt64.div x y)
  | Bit_not `UInt64 -> fun[@warning"-partial-match"] [UInt64_lit x] -> UInt64_lit (UInt64.lognot x)
  | Bit_and `UInt64 -> fun[@warning"-partial-match"] [UInt64_lit x; UInt64_lit y] -> UInt64_lit (UInt64.logand x y)
  | Bit_or `UInt64 -> fun[@warning"-partial-match"] [UInt64_lit x; UInt64_lit y] -> UInt64_lit (UInt64.logor x y)
  | Bit_xor `UInt64 -> fun[@warning"-partial-match"] [UInt64_lit x; UInt64_lit y] -> UInt64_lit (UInt64.logxor x y)
  | Bit_shl `UInt64 -> fun[@warning"-partial-match"] [UInt64_lit x; UInt64_lit y] -> UInt64_lit (UInt64.(shift_left x (to_int_opt y |> Option.get)))
  | Bit_shr `UInt64 -> fun[@warning"-partial-match"] [UInt64_lit x; UInt64_lit y] -> UInt64_lit (UInt64.(shift_right x (to_int_opt y |> Option.get)))

  | Eq `Int8 -> fun[@warning"-partial-match"] [Int8_lit x; Int8_lit y] -> Bool_lit (Int8.equal x y)
  | Ne `Int8 -> fun[@warning"-partial-match"] [Int8_lit x; Int8_lit y] -> Bool_lit (Int8.O.(x <> y))
  | Le `Int8 -> fun[@warning"-partial-match"] [Int8_lit x; Int8_lit y] -> Bool_lit (Int8.O.(x <= y))
  | Lt `Int8 -> fun[@warning"-partial-match"] [Int8_lit x; Int8_lit y] -> Bool_lit (Int8.O.(x < y))
  | Gt `Int8 -> fun[@warning"-partial-match"] [Int8_lit x; Int8_lit y] -> Bool_lit (Int8.O.(x < y))
  | Ge `Int8 -> fun[@warning"-partial-match"] [Int8_lit x; Int8_lit y] -> Bool_lit (Int8.O.(x >= y))
  | Neg `Int8 -> fun[@warning"-partial-match"] [Int8_lit x] -> Int8_lit (Int8.neg x)
  | Add `Int8 -> fun[@warning"-partial-match"] [Int8_lit x; Int8_lit y] -> Int8_lit (Int8.add x y)
  | Sub `Int8 -> fun[@warning"-partial-match"] [Int8_lit x; Int8_lit y] -> Int8_lit (Int8.sub x y)
  | Mul `Int8 -> fun[@warning"-partial-match"] [Int8_lit x; Int8_lit y] -> Int8_lit (Int8.mul x y)
  | Div `Int8 -> fun[@warning"-partial-match"] [Int8_lit x; Int8_lit y] -> Int8_lit (Int8.div x y)
  | Bit_not `Int8 -> fun[@warning"-partial-match"] [Int8_lit x] -> Int8_lit (Int8.lognot x)
  | Bit_and `Int8 -> fun[@warning"-partial-match"] [Int8_lit x; Int8_lit y] -> Int8_lit (Int8.logand x y)
  | Bit_or `Int8 -> fun[@warning"-partial-match"] [Int8_lit x; Int8_lit y] -> Int8_lit (Int8.logor x y)
  | Bit_xor `Int8 -> fun[@warning"-partial-match"] [Int8_lit x; Int8_lit y] -> Int8_lit (Int8.logxor x y)
  | Bit_shl `Int8 -> fun[@warning"-partial-match"] [Int8_lit x; Int8_lit y] -> Int8_lit (Int8.(shift_left x (to_int y)))
  | Bit_shr `Int8 -> fun[@warning"-partial-match"] [Int8_lit x; Int8_lit y] -> Int8_lit (Int8.(shift_right x (to_int y)))

  | Eq `Int16 -> fun[@warning"-partial-match"] [Int16_lit x; Int16_lit y] -> Bool_lit (Int16.equal x y)
  | Ne `Int16 -> fun[@warning"-partial-match"] [Int16_lit x; Int16_lit y] -> Bool_lit (Int16.O.(x <> y))
  | Le `Int16 -> fun[@warning"-partial-match"] [Int16_lit x; Int16_lit y] -> Bool_lit (Int16.O.(x <= y))
  | Lt `Int16 -> fun[@warning"-partial-match"] [Int16_lit x; Int16_lit y] -> Bool_lit (Int16.O.(x < y))
  | Gt `Int16 -> fun[@warning"-partial-match"] [Int16_lit x; Int16_lit y] -> Bool_lit (Int16.O.(x < y))
  | Ge `Int16 -> fun[@warning"-partial-match"] [Int16_lit x; Int16_lit y] -> Bool_lit (Int16.O.(x >= y))
  | Neg `Int16 -> fun[@warning"-partial-match"] [Int16_lit x] -> Int16_lit (Int16.neg x)
  | Add `Int16 -> fun[@warning"-partial-match"] [Int16_lit x; Int16_lit y] -> Int16_lit (Int16.add x y)
  | Sub `Int16 -> fun[@warning"-partial-match"] [Int16_lit x; Int16_lit y] -> Int16_lit (Int16.sub x y)
  | Mul `Int16 -> fun[@warning"-partial-match"] [Int16_lit x; Int16_lit y] -> Int16_lit (Int16.mul x y)
  | Div `Int16 -> fun[@warning"-partial-match"] [Int16_lit x; Int16_lit y] -> Int16_lit (Int16.div x y)
  | Bit_not `Int16 -> fun[@warning"-partial-match"] [Int16_lit x] -> Int16_lit (Int16.lognot x)
  | Bit_and `Int16 -> fun[@warning"-partial-match"] [Int16_lit x; Int16_lit y] -> Int16_lit (Int16.logand x y)
  | Bit_or `Int16 -> fun[@warning"-partial-match"] [Int16_lit x; Int16_lit y] -> Int16_lit (Int16.logor x y)
  | Bit_xor `Int16 -> fun[@warning"-partial-match"] [Int16_lit x; Int16_lit y] -> Int16_lit (Int16.logxor x y)
  | Bit_shl `Int16 -> fun[@warning"-partial-match"] [Int16_lit x; Int16_lit y] -> Int16_lit (Int16.(shift_left x (to_int y)))
  | Bit_shr `Int16 -> fun[@warning"-partial-match"] [Int16_lit x; Int16_lit y] -> Int16_lit (Int16.(shift_right x (to_int y)))

  | Eq `Int32 -> fun[@warning"-partial-match"] [Int32_lit x; Int32_lit y] -> Bool_lit (Int32.equal x y)
  | Ne `Int32 -> fun[@warning"-partial-match"] [Int32_lit x; Int32_lit y] -> Bool_lit (Int32.O.(x <> y))
  | Le `Int32 -> fun[@warning"-partial-match"] [Int32_lit x; Int32_lit y] -> Bool_lit (Int32.O.(x <= y))
  | Lt `Int32 -> fun[@warning"-partial-match"] [Int32_lit x; Int32_lit y] -> Bool_lit (Int32.O.(x < y))
  | Gt `Int32 -> fun[@warning"-partial-match"] [Int32_lit x; Int32_lit y] -> Bool_lit (Int32.O.(x < y))
  | Ge `Int32 -> fun[@warning"-partial-match"] [Int32_lit x; Int32_lit y] -> Bool_lit (Int32.O.(x >= y))
  | Neg `Int32 -> fun[@warning"-partial-match"] [Int32_lit x] -> Int32_lit (Int32.neg x)
  | Add `Int32 -> fun[@warning"-partial-match"] [Int32_lit x; Int32_lit y] -> Int32_lit (Int32.add x y)
  | Sub `Int32 -> fun[@warning"-partial-match"] [Int32_lit x; Int32_lit y] -> Int32_lit (Int32.sub x y)
  | Mul `Int32 -> fun[@warning"-partial-match"] [Int32_lit x; Int32_lit y] -> Int32_lit (Int32.mul x y)
  | Div `Int32 -> fun[@warning"-partial-match"] [Int32_lit x; Int32_lit y] -> Int32_lit (Int32.div x y)
  | Bit_not `Int32 -> fun[@warning"-partial-match"] [Int32_lit x] -> Int32_lit (Int32.lognot x)
  | Bit_and `Int32 -> fun[@warning"-partial-match"] [Int32_lit x; Int32_lit y] -> Int32_lit (Int32.logand x y)
  | Bit_or `Int32 -> fun[@warning"-partial-match"] [Int32_lit x; Int32_lit y] -> Int32_lit (Int32.logor x y)
  | Bit_xor `Int32 -> fun[@warning"-partial-match"] [Int32_lit x; Int32_lit y] -> Int32_lit (Int32.logxor x y)
  | Bit_shl `Int32 -> fun[@warning"-partial-match"] [Int32_lit x; Int32_lit y] -> Int32_lit (Int32.(shift_left x (to_int_opt y |> Option.get)))
  | Bit_shr `Int32 -> fun[@warning"-partial-match"] [Int32_lit x; Int32_lit y] -> Int32_lit (Int32.(shift_right x (to_int_opt y |> Option.get)))

  | Eq `Int64 -> fun[@warning"-partial-match"] [Int64_lit x; Int64_lit y] -> Bool_lit (Int64.equal x y)
  | Ne `Int64 -> fun[@warning"-partial-match"] [Int64_lit x; Int64_lit y] -> Bool_lit (Int64.O.(x <> y))
  | Le `Int64 -> fun[@warning"-partial-match"] [Int64_lit x; Int64_lit y] -> Bool_lit (Int64.O.(x <= y))
  | Lt `Int64 -> fun[@warning"-partial-match"] [Int64_lit x; Int64_lit y] -> Bool_lit (Int64.O.(x < y))
  | Gt `Int64 -> fun[@warning"-partial-match"] [Int64_lit x; Int64_lit y] -> Bool_lit (Int64.O.(x < y))
  | Ge `Int64 -> fun[@warning"-partial-match"] [Int64_lit x; Int64_lit y] -> Bool_lit (Int64.O.(x >= y))
  | Neg `Int64 -> fun[@warning"-partial-match"] [Int64_lit x] -> Int64_lit (Int64.neg x)
  | Add `Int64 -> fun[@warning"-partial-match"] [Int64_lit x; Int64_lit y] -> Int64_lit (Int64.add x y)
  | Sub `Int64 -> fun[@warning"-partial-match"] [Int64_lit x; Int64_lit y] -> Int64_lit (Int64.sub x y)
  | Mul `Int64 -> fun[@warning"-partial-match"] [Int64_lit x; Int64_lit y] -> Int64_lit (Int64.mul x y)
  | Div `Int64 -> fun[@warning"-partial-match"] [Int64_lit x; Int64_lit y] -> Int64_lit (Int64.div x y)
  | Bit_not `Int64 -> fun[@warning"-partial-match"] [Int64_lit x] -> Int64_lit (Int64.lognot x)
  | Bit_and `Int64 -> fun[@warning"-partial-match"] [Int64_lit x; Int64_lit y] -> Int64_lit (Int64.logand x y)
  | Bit_or `Int64 -> fun[@warning"-partial-match"] [Int64_lit x; Int64_lit y] -> Int64_lit (Int64.logor x y)
  | Bit_xor `Int64 -> fun[@warning"-partial-match"] [Int64_lit x; Int64_lit y] -> Int64_lit (Int64.logxor x y)
  | Bit_shl `Int64 -> fun[@warning"-partial-match"] [Int64_lit x; Int64_lit y] -> Int64_lit (Int64.(shift_left x (to_int_opt y |> Option.get)))
  | Bit_shr `Int64 -> fun[@warning"-partial-match"] [Int64_lit x; Int64_lit y] -> Int64_lit (Int64.(shift_right x (to_int_opt y |> Option.get)))

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
