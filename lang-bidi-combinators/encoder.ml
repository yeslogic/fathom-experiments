(** The type of an encoder, parameterized by:

    - ['ctx]: the context of the encoder
    - ['a]: the type of the decoded value (used for dependent encoders)
*)
type ('ctx, 'a) t = Buffer.t -> 'ctx -> 'a option

let ( let+ ) x f = Option.map f x
let ( let* ) = Option.bind

let pure (type ctx a) (x : a) : (ctx, a) t =
  fun _ _ -> Some x

let map (type ctx a b) (f : a -> b) (x : (ctx, a) t) : (ctx, b) t =
  fun buf c ->
    let+ x = x buf c in
    f x

let both (type ctx a b) (x : (ctx, a) t) (y : (ctx, b) t) : (ctx, a * b) t =
  fun buf c ->
    let* x = x buf c in
    let+ y = y buf c in
    (x, y)

let apply (type ctx a b) (f : (ctx, a -> b) t) (x : (ctx, a) t) : (ctx, b) t =
  both f x |> map (fun (f, x) -> f x)

let bind (type ctx a b) (x : (ctx, a) t) (y : a -> (ctx, b) t) : (ctx, b) t =
  fun buf c ->
    let* x = x buf c in
    (y x) buf c

let fail (type ctx a) : (ctx, a) t =
  fun _ _ ->
    failwith "encoding fail format"

let dimap (type a b c c') (f : a -> b) (g : c' -> c) (x : (c, a) t) : (c', b) t =
  fun buf c ->
    let+ x = x buf (g c) in
    f x

let comap (type ctx ctx' a) (f : ctx' -> ctx) (x : (ctx, a) t) : (ctx', a) t =
  fun buf c ->
    x buf (f c)

module Syntax = struct

  let ( <$> ) = map
  let ( <*> ) = apply

  let ( @= ) = comap

  let ( let+ ) x f = map f x
  let ( and+ ) = both
  let ( let* ) = bind
  let ( and* ) = both

end
