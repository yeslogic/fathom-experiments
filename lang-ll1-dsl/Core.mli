type ty
(** The type of types.
    {@text[
    t ::=
      | Unit
      | Byte
      | Pair t t
    ]}
*)

type expr
(** The type of expressions.

    {@text[
    e ::=
      | ()
      | b
      | e, e
    ]}
*)

type format
(** Format descriptions.

    {@text[
    f ::=
      | i
      | ()
      | bs
      | f, f
      | f | f
      | map @t (x => e) f
    ]}
*)

type program
(** The type of programs.

    {@text[
    p ::=
      | ∅
      | def x : Format := f; p
    ]}
*)

val pp_print_program : Format.formatter -> program -> unit
(** Pretty print a program *)

module Semantics : sig

  type vexpr

  type local_env

  val eval : local_env -> expr -> vexpr
  val quote : vexpr -> expr
  val normalise : local_env -> expr -> expr

end

module Refiner : sig
  (** Trusted interface for constructing programs in the core language. *)


  (** {1 Forms of judgement} *)

  type item_var
  (** A reference to a top-level item.

      {@text[i : Format ∈ S]}
  *)

  type local_var
  (** A reference to a local binding.

      {@text[x : t ∈ L]}
  *)

  type is_program
  (** Well-formed programs.

      {@text[S ⊢ program(p)]}
  *)

  type is_format
  (** Well-formed formats.

      {@text[S ⊢ format(f)]}
  *)

  type synth_ty
  (** Synthesise the type of an expression.

      {@text[S; L ⊢ synth(e) ⇒ t]}
  *)

  type check_ty
  (** Check an expression against a type annotation.

      {@text[S; L ⊢ check(e) ⇐ t]}
  *)


  (** {1 Running rules} *)

  val run_is_program : is_program -> program
  (** Check that a program is well-formed, if successful returning the program
      in the core language.
  *)


  (** {1 Inference rules} *)

  module Program : sig
    (** Rules for constructing programs. *)

    val empty : is_program
    (** The empty program.

        {@text[
        ───────────────────
          S ⊢ program(∅)
        ]}
    *)

    val def_format : string * is_format -> (item_var -> is_program) -> is_program
    (** A program with a format definition.

        {@text[
          S ⊢ format(f)
          i : Format, S ⊢ program(p)
        ───────────────────────────────
          S ⊢ program(def i : Format := f; p)
        ]}
    *)

  end

  module Format : sig
    (** Rules for constructing format descriptions. *)

    exception AmbiguousConcatenation
    (** Thrown if calling {!cat} results in an ambiguity. *)

    exception AmbiguousAlternation
    (** Thrown if calling {!alt} results in an ambiguity. *)

    val empty : is_format
    (** The empty format.

        {@text[
        ───────────────────
          S ⊢ format(())
        ]}
    *)

    val item : item_var -> is_format
    (** Item formats.

        {@text[
          i : Format ∈ S
        ──────────────────
          S ⊢ format(i)
        ]}
    *)

    val byte : ByteSet.t -> is_format
    (** Byte formats.

        {@text[
        ──────────────────
          S ⊢ format(bs)
        ]}
    *)

    val cat : is_format -> is_format -> is_format
    (** Concatenated formats.

        {@text[
          S ⊢ format(f₀)
          S ⊢ format(f₁)
          separate(f₀, f₁)
        ────────────────────────
          S ⊢ format(f₀, f₁)
        ]}

        @raise AmbiguousConcatenation if the formats are not separate
    *)

    val alt : is_format -> is_format -> is_format
    (** Alternation formats.

        {@text[
          S ⊢ format(f₀)
          S ⊢ format(f₁)
          non-overlapping(f₀, f₁)
          repr(f₀) ≡ repr(f₁)
        ───────────────────────────────
          S ⊢ format(f₀ | f₁)
        ]}

        @raise AmbiguousAlternation if the formats overlap
    *)

    val map : (string * (local_var -> synth_ty)) -> is_format -> is_format
    (**  Map formats.

        {@text[
          S ⊢ format(f)
          S; x : repr(f), L ⊢ synth-ty(e) ⇒ t
        ───────────────────────────────────────
          S ⊢ format(map @t (x => e) f)
        ]}
    *)

  end

  module Structural : sig

    val local : local_var -> synth_ty
    (** Local variables.

        {@text[
          x : t ∈ L
        ───────────────────────
          S; L ⊢ synth(x) ⇒ t
        ]}
    *)

  end

  module Unit : sig

    val intro : synth_ty
    (** Unit introduction.

        {@text[
        ─────────────────────────
          S; L ⊢ synth(()) ⇒ Unit
        ]}
    *)

  end

  module Byte : sig

    val intro : char -> synth_ty
    (** Byte introduction.

        {@text[
        ────────────────────────
          S; L ⊢ synth(b) ⇒ Byte
        ]}
    *)

  end

  module Pair : sig

    val intro : synth_ty -> synth_ty -> synth_ty
    (** Pair introduction.

        {@text[
          S; L ⊢ synth(e₀) ⇒ t₀
          S; L ⊢ synth(e₁) ⇒ t₁
        ────────────────────────────────────
          S; L ⊢ synth(e₀, e₁) ⇒ Pair t₀ t₁
        ]}
    *)

  end

end

module Decode : sig
  (** Tree-walking parser interpreter. *)

  exception DecodeFailure of int

  val run : program -> format -> bytes -> int -> int * Semantics.vexpr

end
