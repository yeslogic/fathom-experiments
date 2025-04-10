(** Elaboration from the surface language to the core language. *)

open Syntax

exception Error of loc * string

val check_program : item list -> Core.program
