(** A set of bytes in the range [0..255] *)

type t
(** The type of byte sets *)


(** {1 Construction} *)

val make : (char -> bool) -> t
(** [make f] constructs a byte set from the character predicate [f]. *)

val empty : t
(** The empty byte set. *)

val full : t
(** The full byte set. *)

val singleton : char -> t
(** [singleton x] returns the one-element byte set containing only [x]. *)

val range : char -> char -> t
(** [of_range c0 c1] returns a byte set containing all bytes in the range
    [c0..c1] (inclusive). *)


(** {1 Operators} *)

val union : t -> t -> t
(** [union s0 s1] returns a byte set containing all the bytes that are members
    of either [s0] or [s1], or both. *)

val inter : t -> t -> t
(** [inter s0 s1] returns a byte set containing the bytes that are members of
    both [s0] and [s1]. *)

val diff : t -> t -> t
(** [diff s0 s1] returns a byte set containing the bytes that are members of
    [s0] and not members of [s1]. *)

val neg : t -> t
(** [neg s] returns a byte set containing all the bytes that are not in [s] *)


(** {1 Predicates and comparisons} *)

val mem : char -> t -> bool
(** [mem x s] tests whether [x] is a member of the byte set [s]. *)

val equal : t -> t -> bool
(** [equal s0 s1] tests whether [s0] and [s1] contain the same bytes. *)

val is_empty : t -> bool
(** [is_empty s] tests whether [s] is empty. *)

val disjoint : t -> t -> bool
(** [disjoint s0 s1] tests if [s0] and [s1] have no bytes in common. *)


(** {1 Converting} *)

val of_string : string -> t
(** [of_string s] returns a byte set containing all the characters in [s] *)

val of_bytes : bytes -> t
(** [of_bytes s] returns a byte set containing all the bytes in [s] *)


(** {1 Formatting} *)

val pp_print : Format.formatter -> t -> unit
(** [pp_print ppf s] pretty prints [s] using the formatter [ppf] *)
