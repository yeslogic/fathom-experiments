(** The surface language. *)

(** {1 Syntax} *)

(** {2 Terms} *)

type tm

type bound =
  | Inclusive of int
  | Exclusive of int

val empty : tm
val name : string -> tm
val byte : int -> tm
val byte_range : bound option -> bound option -> tm
val not : tm -> tm
val cat : tm -> tm -> tm
val alt : tm -> tm -> tm

(** {2 Programs} *)

type program

val program : (string * tm) list -> program


(** {1 Elaboration} *)

val elab_program : program -> Core.Refiner.is_program
