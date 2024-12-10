open Brr

module Attr = struct

  type t = El.t -> unit

  (** Attribute setters *)

  let set_true (name : string) : t =
    El.set_at (Jstr.v name) (Some Jstr.empty)

  let set_string (name : string) (value : string) : t =
    El.set_at (Jstr.v name) (Some (Jstr.v value))

  let set_handler (type a) (type' : a Ev.type') (handler : a Ev.t -> unit) (elem : El.t) =
    ignore (El.as_target elem |> Ev.listen type' handler : Ev.listener)

  let set_if (b : bool) (attr : t) (elem : El.t) =
    if b then attr elem

  (** Global attributes *)

  let id = set_string "id"                  (* string *)
  let spellcheck = set_string "spellcheck"  (* "true" | "default" | "false" *)

  (** Element-specific attributes *)

  let selected = set_true "selected"
  let wrap = set_string "wrap"              (* "hard" | "soft" | "wrap" *)

  (** Events *)

  let on_click = set_handler Ev.click
  let on_input = set_handler Ev.input

end

let text content = El.txt' content

let elem ?(d : El.document option) (name : string) (attrs : Attr.t list) (children : El.t list) : El.t =
  let elem = El.v ?d (Jstr.v name) children in
  attrs |> List.iter (fun attr -> attr elem);
  elem

let button ?d = elem ?d "button"
let div ?d = elem ?d "div"
let nav ?d = elem ?d "nav"
let option ?d = elem ?d "option"
let pre ?d = elem ?d "pre"
let select ?d = elem ?d "select"
let textarea ?d = elem ?d "textarea"
