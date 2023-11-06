type ty
type expr
type format

type program
(** The type of programs. *)

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

  type 'e is_program = (program, 'e) ItemM.m
  type 'e is_format = (format, 'e) ItemM.m
  type 'e is_ty = (ty, 'e) ItemM.m
  type 'e synth_ty = (expr * ty, 'e) LocalM.m
  type 'e check_ty = ty -> (expr, 'e) LocalM.m


  (** {1 Inference rules} *)

  module Program : sig

    val empty : 'e is_program
    val def_format : string * 'e is_format -> (item_var -> 'e is_program) -> 'e is_program

  end

  module Format : sig

    val empty : 'e is_format
    val item : item_var -> 'e is_format
    val fail : 'e is_ty -> 'e is_format
    val byte : ByteSet.t -> 'e is_format
    val seq : Void.t is_format -> Void.t is_format -> [`AmbiguousFormat] is_format
    val union : Void.t is_format -> Void.t is_format -> [`AmbiguousFormat | `ReprMismatch of ty * ty] is_format
    val map : (string * (local_var -> 'e synth_ty)) -> 'e is_format -> 'e is_format

  end

  module Structural : sig

    val local : local_var -> 'e synth_ty
    val conv : Void.t synth_ty -> [`TypeMismatch of ty * ty] check_ty
    val ann : 'e check_ty -> 'e is_ty -> 'e synth_ty

  end

  module Unit : sig

    val form : 'e is_ty
    val intro : 'e synth_ty

  end

  module Byte : sig

    val form : 'e is_ty
    val intro : char -> 'e synth_ty

  end

  module Pair : sig

    val form : 'e is_ty -> 'e is_ty -> 'e is_ty
    val intro : 'e synth_ty -> 'e synth_ty -> 'e synth_ty
    val fst : Void.t synth_ty -> [`UnexpectedType] synth_ty
    val snd : Void.t synth_ty -> [`UnexpectedType] synth_ty

  end

end
