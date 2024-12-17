type 'a t = input:bytes -> pos:int -> (int * 'a, int) result

let run (type a) (x : a t) : a t =
  x


let ( let* ) = Result.bind


let pure (type a) (x : a) : a t =
  fun ~input:_ ~pos ->
    Ok (pos, x)

let map (type a b) (f : a -> b) (x : a t) : b t =
  fun ~input ~pos ->
    let* (pos, x) = x ~input ~pos in
    Ok (pos, f x)

let both (type a b) (x : a t) (y : b t) : (a * b) t =
  fun ~input ~pos ->
    let* (pos, x) = x ~input ~pos in
    let* (pos, y) = y ~input ~pos in
    Ok (pos, (x, y))

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
        let* (pos, xs) = repeat_len (Int64.pred len) ~input ~pos in
        Ok (pos, x :: xs)
  in
  repeat_len len
