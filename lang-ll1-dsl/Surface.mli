type bound =
  | Inclusive of int
  | Exclusive of int

type range = {
  start : bound option;
  stop : bound option;
}

type sign =
  | Pos
  | Neg

type tm =
  | Unit
  | Name of string
  | Byte of sign * int
  | ByteRange of sign * range
  | Cat of tm * tm
  | Alt of tm * tm

type program = {
  items : (string * tm) list;
}

module Elab : sig

  type context

  val empty_context : context

  val elab_program : context -> program -> Core.Refiner.is_program
  val elab_format : context -> tm -> Core.Refiner.is_format

end
