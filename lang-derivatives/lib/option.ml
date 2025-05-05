(** Extensions to [Stdlib.Option] *)

include Stdlib.Option

let both (type a b) (o1 : a t) (o2 : b t) : (a * b) t =
  match o1, o2 with
  | Some x1, Some x2 -> Some (x1, x2)
  | None, _ | _, None -> None

let alt (type a) (o1 : a t) (o2 : unit -> a t) : a t =
  match o1 with
  | Some x -> Some x
  | None -> o2 ()

module Notation = struct

  let ( let* ) = bind
  let ( let+ ) x f = map f x
  let ( and+ ) = both

end
