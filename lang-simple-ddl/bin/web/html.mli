(** Attributes for HTML elements *)
module Attr : sig

  type t = Brr.El.t -> unit
  (** The type of attribute setters *)

  (** {1 Attribute setters} *)

  val set_true : string -> t
  val set_string : string -> string -> t
  val set_handler : 'a. 'a Brr.Ev.type' -> ('a Brr.Ev.t -> unit) -> t
  val set_if : bool -> t -> t

  (** {1 Global Attributes} *)

  val id : string -> t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/id} id} *)

  val spellcheck : string -> t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/spellcheck} spellcheck} *)

  (** {1 Element-specific attributes} *)

  val selected : t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option#selected} selected} *)

  val wrap : string -> t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea#wrap} wrap} *)

  (** {1 Event handler attributes} *)

  val on_click : (Brr.Ev.Mouse.t Brr.Ev.t -> unit) -> t
  val on_input : (Brr.Ev.Input.t Brr.Ev.t -> unit) -> t

end

(** DOM Elements *)

val text : string -> Brr.El.t

val elem : ?d:Brr.El.document -> string -> Attr.t list -> Brr.El.t list -> Brr.El.t

val button : ?d:Brr.El.document -> Attr.t list -> Brr.El.t list -> Brr.El.t
val div : ?d:Brr.El.document -> Attr.t list -> Brr.El.t list -> Brr.El.t
val nav : ?d:Brr.El.document -> Attr.t list -> Brr.El.t list -> Brr.El.t
val option : ?d:Brr.El.document -> Attr.t list -> Brr.El.t list -> Brr.El.t
val pre : ?d:Brr.El.document -> Attr.t list -> Brr.El.t list -> Brr.El.t
val select : ?d:Brr.El.document -> Attr.t list -> Brr.El.t list -> Brr.El.t
val textarea : ?d:Brr.El.document -> Attr.t list -> Brr.El.t list -> Brr.El.t
