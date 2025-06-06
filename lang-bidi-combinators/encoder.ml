type ('c, 'a) t = Buffer.t -> 'c -> 'a option

let ( let+ ) x f = Option.map f x
let ( let* ) = Option.bind

let pure (type a c) (x : a) : (c, a) t =
  fun _ _ -> Some x

let map (type a b c) (f : a -> b) (x : (c, a) t) : (c, b) t =
  fun buf c ->
    let+ x = x buf c in
    f x

let both (type a b c) (x : (c, a) t) (y : (c, b) t) : (c, a * b) t =
  fun buf c ->
    let* x = x buf c in
    let+ y = y buf c in
    (x, y)

let apply (type a b c) (f : (c, a -> b) t) (x : (c, a) t) : (c, b) t =
  both f x |> map (fun (f, x) -> f x)

let bind (type a b c) (x : (c, a) t) (y : a -> (c, b) t) : (c, b) t =
  fun buf c ->
    let* x = x buf c in
    (y x) buf c

let fail (type a c) : (c, a) t =
  fun _ _ -> None

let alt (type a c) (x : (c, a) t) (y : (c, a) t) : (c, a) t =
  fun buf pos ->
    match x buf pos with
    | None -> y buf pos
    | Some x -> Some x

let dimap (type a b c d) (f : a -> b) (g : d -> c) (x : (c, a) t) : (d, b) t =
  fun buf c ->
    let+ x = x buf (g c) in
    f x

let comap (type a c d) (f : d -> c) (x : (c, a) t) : (d, a) t =
  fun buf c ->
    x buf (f c)

let const (type a c) (x : (a, a) t) (expected : a) : (c, unit) t =
  fun buf _ ->
    let+ _ = x buf expected in ()

module Syntax = struct

  let ( <$> ) = map
  let ( <*> ) = apply
  let ( </> ) = alt

  let ( @= ) = comap

  let ( let+ ) x f = map f x
  let ( and+ ) = both
  let ( let* ) = bind
  let ( and* ) = both

end
