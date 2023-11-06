type ty
type expr
type format

type program
(** The type of programs. *)

val pp_print_ty : Format.formatter -> ty -> unit
(** Pretty print a type *)

val pp_print_program : Format.formatter -> program -> unit
(** Pretty print a program *)

module Semantics : sig
  (** The semantics of the core language. *)

  (** {1 Expression semantics} *)

  type vexpr
  (** Evaluated expressions (i.e. values). *)

  type local_env

  val eval : local_env -> expr -> vexpr
  (** Evaluate an expression. *)

  val quote : vexpr -> expr
  (** Quote a value back into an expression. *)

  val normalise : local_env -> expr -> expr
  (** Normalisation by evaluation. *)


  (** {1 Decode semantics} *)

  exception DecodeFailure of int

  val decode : program -> format -> bytes -> int -> int * vexpr

end

module Refiner : sig
  (** Trusted interface for constructing programs in the core language. *)


  (** {1 Context effects} *)

  type 'a item_m
  type 'a local_m
  type ('a, 'e) item_err_m = ('a, 'e) result item_m
  type ('a, 'e) local_err_m = ('a, 'e) result local_m

  val handle_item : ('e -> 'a item_m) -> ('a, 'e) item_err_m -> 'a item_m
  val handle_local : ('e -> 'a local_m) -> ('a, 'e) local_err_m -> 'a local_m

  val run_item : 'a item_m -> 'a
  val run_local : 'a local_m -> 'a


  (** {1 Forms of judgement} *)

  type item_var
  type local_var

  type is_program = program item_m
  type is_format = format local_m
  type is_ty = ty item_m
  type synth_ty = (expr * ty) local_m
  type check_ty = ty -> expr local_m

  type 'e is_program_err = (program, 'e) item_err_m
  type 'e is_format_err = (format, 'e) local_err_m
  type 'e is_ty_err = (ty, 'e) item_err_m
  type 'e synth_ty_err = (expr * ty, 'e) local_err_m
  type 'e check_ty_err = ty -> (expr, 'e) local_err_m


  (** {1 Inference rules} *)

  module Program : sig

    val empty : is_program
    val def_ty : string * is_ty -> (item_var -> is_program) -> is_program
    val def_format : string * is_format -> (item_var -> is_program) -> is_program
    val def_expr : string * is_ty * check_ty -> (item_var -> is_program) -> is_program

  end

  module Format : sig

    val item : item_var -> [`FormatExpected | `UnboundVariable] is_format_err
    val fail : is_ty -> is_format
    val byte : ByteSet.t -> is_format
    val seq : is_format -> is_format -> [`AmbiguousFormat] is_format_err
    val union : is_format -> is_format -> [`AmbiguousFormat | `ReprMismatch of ty * ty] is_format_err
    val pure : synth_ty -> is_format
    val map : (string * (local_var -> synth_ty)) -> is_format -> is_format
    val flat_map : (string * (local_var -> is_format)) -> is_format -> [`AmbiguousFormat] is_format_err

    val repr : is_format -> is_ty

  end

  module Structural : sig

    val item_ty : item_var -> [`TypeExpected | `UnboundVariable] is_ty_err
    val item_expr : item_var -> [`ExprExpected | `UnboundVariable] synth_ty_err
    val local : local_var -> [`UnboundVariable] synth_ty_err
    val conv : synth_ty -> [`TypeMismatch of ty * ty] check_ty_err
    val ann : check_ty -> is_ty -> synth_ty

  end

  module Unit : sig

    val form : is_ty
    val intro : synth_ty

  end

  module Byte : sig

    val form : is_ty
    val intro : char -> synth_ty

  end

  module Pair : sig

    val form : is_ty -> is_ty -> is_ty
    val intro : synth_ty -> synth_ty -> synth_ty
    val fst : synth_ty -> [`UnexpectedType] synth_ty_err
    val snd : synth_ty -> [`UnexpectedType] synth_ty_err

  end

  module Record : sig

    val form_empty : is_ty
    val form : (string * is_ty) list -> [`DuplicateFieldLabel of string] is_ty_err
    val intro_empty : synth_ty
    val intro : (string * synth_ty) list -> [`DuplicateFieldLabel of string] synth_ty_err
    val proj : synth_ty -> string -> [`UnknownFieldLabel of ty] synth_ty_err

  end

end
