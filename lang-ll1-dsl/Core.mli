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

module Refiner : sig
  (** Trusted interface for constructing programs in the core language. *)


  type item_var
  (** A reference to a top-level item. *)


  (** {1 Forms of judgement} *)

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

      {@text[S ⊢ synth(e) ⇒ t]}
  *)

  type check_ty
  (** Check an expression against a type annotation.

      {@text[S ⊢ check(e) ⇐ t]}
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
    *)

  end

  module Unit : sig

    val intro : synth_ty
    (** Unit introduction.

        {@text[
        ─────────────────────────
          S ⊢ synth(()) ⇒ Unit
        ]}
    *)

  end

  module Byte : sig

    val intro : char -> synth_ty
    (** Byte introduction.

        {@text[
        ────────────────────────
          S ⊢ synth(b) ⇒ Byte
        ]}
    *)

  end

  module Pair : sig

    val intro : synth_ty -> synth_ty -> synth_ty
    (** Pair introduction.

        {@text[
          S ⊢ synth(e₀) ⇒ t₀
          S ⊢ synth(e₁) ⇒ t₁
        ────────────────────────────────────
          S ⊢ synth(e₀, e₁) ⇒ Pair t₀ t₁
        ]}
    *)

  end

end

module Decode : sig
  (** Tree-walking parser interpreter. *)

  exception DecodeFailure of int

  val run : program -> format -> bytes -> int -> int * expr

end
