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
      | x
      | e : t
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
  (** A reference to a top-level item.

      {@text[i : Format ∈ S]}
  *)

  type local_var
  (** A reference to a local binding.

      {@text[x : t ∈ L]}
  *)

  type 'e is_program = (program, 'e) ItemM.m
  (** Well-formed programs.

      {@text[S ⊢ program(p)]}
  *)

  type 'e is_format = (format, 'e) ItemM.m
  (** Well-formed formats.

      {@text[S ⊢ format(f)]}
  *)

  type 'e is_ty
  (** Well-formed types.

      {@text[⊢ type(t)]}
  *)

  type 'e synth_ty
  (** Synthesise the type of an expression.

      {@text[S; L ⊢ synth(e) ⇒ t]}
  *)

  type 'e check_ty
  (** Check an expression against a type annotation.

      {@text[S; L ⊢ check(e) ⇐ t]}
  *)


  (** {1 Inference rules} *)

  module Program : sig
    (** Rules for constructing programs. *)

    val empty : 'e is_program
    (** The empty program.

        {@text[
        ───────────────────
          S ⊢ program(∅)
        ]}
    *)

    val def_format : string * 'e is_format -> (item_var -> 'e is_program) -> 'e is_program
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

    val empty : 'e is_format
    (** The empty format.

        {@text[
        ───────────────────
          S ⊢ format(())
        ]}
    *)

    val item : item_var -> 'e is_format
    (** Item formats.

        {@text[
          i : Format ∈ S
        ──────────────────
          S ⊢ format(i)
        ]}
    *)

    val byte : ByteSet.t -> 'e is_format
    (** Byte formats.

        {@text[
        ──────────────────
          S ⊢ format(bs)
        ]}
    *)

    val cat : Void.t is_format -> Void.t is_format -> [`AmbiguousFormat] is_format
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

    val alt : Void.t is_format -> Void.t is_format -> [`AmbiguousFormat | `ReprMismatch of ty * ty] is_format
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

    val map : (string * (local_var -> 'e synth_ty)) -> 'e is_format -> 'e is_format
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

    val local : local_var -> 'e synth_ty
    (** Local variables.

        {@text[
          x : t ∈ L
        ───────────────────────
          S; L ⊢ synth(x) ⇒ t
        ]}
    *)

    val conv : Void.t synth_ty ->  [`TypeMismatch of ty * ty] check_ty
    (** Conversion checking.

        {@text[
          S; L ⊢ synth(e) ⇒ t₁
          t₀ ≡ t₁
        ───────────────────────
          S; L ⊢ check(e) ⇐ t₀
        ]}
    *)

    val ann : 'e check_ty -> 'e is_ty -> 'e synth_ty
    (** Annotation rule.

        {@text[
          S; L ⊢ check(e) ⇐ t
        ───────────────────────────
          S; L ⊢ synth(e : t) ⇒ t
        ]}
    *)

  end

  module Unit : sig

    val form : 'e is_ty
    (** Unit formation. **)

    val intro : 'e synth_ty
    (** Unit introduction.

        {@text[
        ─────────────────────────
          S; L ⊢ synth(()) ⇒ Unit
        ]}
    *)

  end

  module Byte : sig

    val form : 'e is_ty
    (** Byte formation. **)

    val intro : char -> 'e synth_ty
    (** Byte introduction.

        {@text[
        ────────────────────────
          S; L ⊢ synth(b) ⇒ Byte
        ]}
    *)

  end

  module Pair : sig

    val form : 'e is_ty -> 'e is_ty -> 'e is_ty
    (** Pair formation. **)

    val intro : 'e synth_ty -> 'e synth_ty -> 'e synth_ty
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
