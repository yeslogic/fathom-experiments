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

  module Void := Basis.Void


  (** {1 Context monads} *)

  module ItemM : sig

    include Basis.IndexedMonad.S

    val handle : ('e_in -> ('a, 'e_out) m) -> ('a, 'e_in) m -> ('a, 'e_out) m
    val throw : 'e -> ('a, 'e) m

    val run : ('a, 'e) m -> ('a, 'e) result

  end

  module LocalM : sig

    include Basis.IndexedMonad.S

    val handle : ('e_in -> ('a, 'e_out) m) -> ('a, 'e_in) m -> ('a, 'e_out) m
    val throw : 'e -> ('a, 'e) m

    val item_m : ('a, 'e) ItemM.m -> ('a, 'e) m

  end


  (** {1 Forms of judgement} *)

  type item_var
  type local_var

  type 'e is_program_err = (program, 'e) ItemM.m
  type 'e is_format_err = (format, 'e) ItemM.m
  type 'e is_ty_err = (ty, 'e) ItemM.m
  type 'e synth_ty_err = (expr * ty, 'e) LocalM.m
  type 'e check_ty_err = ty -> (expr, 'e) LocalM.m

  type is_program = Void.t is_program_err
  type is_format = Void.t is_format_err
  type is_ty = Void.t is_ty_err
  type synth_ty = Void.t synth_ty_err
  type check_ty = Void.t check_ty_err


  (** {1 Inference rules} *)

  module Program : sig

    val empty : is_program
    val def_ty : string * is_ty -> (item_var -> is_program) -> is_program
    val def_format : string * is_format -> (item_var -> is_program) -> is_program
    val def_expr : string * is_ty * check_ty -> (item_var -> is_program) -> is_program

  end

  module Format : sig

    val item : item_var -> [`FormatExpected | `UnboundVariable] is_format_err
    val empty : is_format
    val fail : is_ty -> is_format
    val byte : ByteSet.t -> is_format
    val seq : is_format -> is_format -> [`AmbiguousFormat] is_format_err
    val union : is_format -> is_format -> [`AmbiguousFormat | `ReprMismatch of ty * ty] is_format_err
    val map : (string * (local_var -> synth_ty)) -> is_format -> is_format

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

end
