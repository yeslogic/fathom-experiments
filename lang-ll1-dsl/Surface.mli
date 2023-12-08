(** The surface language. *)

(** {1 Syntax} *)

(** {2 Terms} *)

(** The start and end position in a source file *)
type loc = Lexing.position * Lexing.position

type 'a located

val located : loc -> 'a -> 'a located

type tm_data
type tm = tm_data located

type bound =
  | Open
  | Inclusive of tm
  | Exclusive of tm

val empty : tm_data
val name : string -> tm_data
val int : int -> tm_data
val range : bound -> bound -> tm_data
val not : tm -> tm_data
val seq : tm -> tm -> tm_data
val union : tm -> tm -> tm_data
val action : tm -> (string * tm) -> tm_data
val proj : tm -> string -> tm_data

(** {2 Programs} *)

type program

val program : (string * tm option * tm) list -> program


(** {1 Elaboration} *)

val elab_program : program -> Core.program
