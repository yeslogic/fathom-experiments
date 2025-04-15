(** Extensions to the [Option] module *)

include Stdlib.Option

let both o1 o2 =
  match o1, o2 with
  | Some x1, Some x2 -> Some (x1, x2)
  | None, _ | _, None -> None

let alt o1 o2 =
  match o1 with
  | Some x -> Some x
  | None -> o2 ()

module Notation = struct

  let ( let* ) = bind
  let ( let+ ) x f = map f x
  let ( and+ ) = both

end
