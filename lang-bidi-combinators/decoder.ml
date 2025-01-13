type 'a t = bytes -> int -> ('a * int) option

let ( let+ ) x f = Option.map f x
let ( let* ) = Option.bind

let pure (type a) (x : a) : a t =
  fun _ pos -> Some (x, pos)

let map (type a b) (f : a -> b) (x : a t) : b t =
  fun buf pos ->
    let+ x, pos = x buf pos in
    (f x, pos)

let both (type a b) (x : a t) (y : b t) : (a * b) t =
  fun buf pos ->
    let* (x, pos) = x buf pos in
    let+ (y, pos) = y buf pos in
    ((x, y), pos)

let apply (type a b) (f : (a -> b) t) (x : a t) : b t =
  both f x |> map (fun (f, x) -> f x)

let bind (type a b) (x : a t) (y : a -> b t) : b t =
  fun buf pos ->
    let* (x, pos) = x buf pos in
    (y x) buf pos

let fail (type a) : a t =
  fun _ _ -> None

let alt (type a) (x : a t) (y : a t) : a t =
  fun buf pos ->
    match x buf pos with
    | None -> y buf pos
    | Some (x, pos) -> Some (x, pos)

module Syntax = struct

  let ( <$> ) = map
  let ( <*> ) = apply
  let ( </> ) = alt

  let ( let+ ) x f = map f x
  let ( and+ ) = both
  let ( let* ) = bind
  let ( and* ) = both

end
