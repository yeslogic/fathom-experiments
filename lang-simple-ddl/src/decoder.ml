type 'a t = input:bytes -> pos:int -> (int * 'a, int) result

let run (type a) (x : a t) : a t =
  x


let ( let+ ) x f = Result.map f x
let ( let* ) = Result.bind


let pure (type a) (x : a) : a t =
  fun ~input:_ ~pos ->
    Ok (pos, x)

let map (type a b) (f : a -> b) (x : a t) : b t =
  fun ~input ~pos ->
    x ~input ~pos
    |> Result.map (fun (pos, x) -> (pos, f x))

let both (type a b) (x : a t) (y : b t) : (a * b) t =
  fun ~input ~pos ->
    let* (pos, x) = x ~input ~pos in
    let+ (pos, y) = y ~input ~pos in
    (pos, (x, y))

(** Branch combinator from “Staged selective parser combinators”
    https://doi.org/10.1145/3409002 *)
let branch (type a1 a2 b) (x : (a1, a2) Either.t t) (f1 : (a1 -> b) t) (f2 : (a2 -> b) t) : b t =
  fun ~input ~pos ->
    let* (pos, x) = x ~input ~pos in
    match x with
    | Left x1 -> let+ (pos, f1) = f1 ~input ~pos in (pos, f1 x1)
    | Right x2 -> let+ (pos, f2) = f2 ~input ~pos in (pos, f2 x2)

let bind (type a b) (x : a t) (f : a -> b t) : b t =
  fun ~input ~pos ->
    let* (pos, x) = x ~input ~pos in
    f x ~input ~pos

let fail (type a) : a t =
  fun ~input:_ ~pos ->
    Error pos


let byte (type a) : char t =
  fun ~input ~pos ->
    if pos < Bytes.length input then
      Ok (pos + 1, Bytes.unsafe_get input pos)
    else
      Error pos

let repeat_len (type a) (len : int64) (elem : a t) : a list t =
  let rec repeat_len len =
    fun ~input ~pos ->
      if len <= Int64.zero then
        Ok (pos, [])
      else
        let* (pos, x) = elem ~input ~pos in
        let+ (pos, xs) = repeat_len (Int64.pred len) ~input ~pos in
        (pos, x :: xs)
  in
  repeat_len len
