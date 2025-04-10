(** The start and end position in a source file *)
type loc =
  Lexing.position * Lexing.position

(** Located nodes *)
type 'a located = {
  loc : loc;
  data : 'a;
}

(** Names that bind definitions or parameters *)
type binder =
  string located

(** Unary operators *)
type op1 = [
  | `Neg
  | `Bit_not
]

(** Binary operators *)
type op2 = [
  | [ `Eq | `Ne | `Le | `Lt | `Ge | `Gt ]
  | `Bit_or
  | `Bit_xor
  | `Bit_and
  | [ `Bit_shl | `Bit_shr ]
  | [ `Add | `Sub ]
  | [ `Mul | `Div ]
]

type tm =
  tm_node located

and tm_node =
  | Name of string * arg list                 (* x | x tm ... tm *)
  | Placeholder                               (* _ *)
  | Ann of tm * tm                            (* tm : tm *)
  | Let of binder * tm option * tm * tm       (* let x : tm := tm; tm *)
  | Bind of binder * tm * tm                  (* let x <- tm; tm *)
  | Record_lit of (string located * tm) list  (* { x := tm; ... } *)
  | Int_lit of string                         (* ... | -1 | 0 | 1 | ... *)
  | Proj of tm * string located               (* tm.l *)
  | If_then_else of tm * tm * tm              (* if tm then tm else tm *)
  | Op1 of op1 * tm                           (* op tm *)
  | Op2 of op2 * tm * tm                      (* tm op tm *)

and arg =
  | Anon of tm                                (* tm *)
  | Labelled of string located * tm           (* (l := tm) *)

type format_field =
  | Let of binder * tm option * tm            (* let x : tm := tm *)
  | Bind of binder * tm                       (* let x <- tm *)
  | Let_field of binder * tm option * tm      (* x : tm := tm *)
  | Bind_field of binder * tm                 (* x <- tm *)
  (* TODO: add `where ...` *)

type item =
  | Record_type of binder * (string located * tm) list
  | Record_format of binder * format_field list
  | Format_def of binder * tm
  | Type_def of binder * tm
  | TermDef of binder * tm option * tm

type program =
  item list
