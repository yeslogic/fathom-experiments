(** The surface language. *)

(** {1 Syntax} *)

(** {2 Terms} *)

type tm

type bound =
  | Open
  | Inclusive of tm
  | Exclusive of tm

val empty : tm
val name : string -> tm
val int : int -> tm
val range : bound -> bound -> tm
val not : tm -> tm
val seq : tm -> tm -> tm
val union : tm -> tm -> tm
val action : tm -> (string * tm) -> tm

(** {2 Programs} *)

type program

val program : (string * tm option * tm) list -> program


(** {1 Elaboration} *)

val elab_program : program -> Core.program
