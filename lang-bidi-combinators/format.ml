(** The type of a format, parameterized by:

    - ['ctx]: the context of the encoder
    - ['a]: the type of the decoded value (used for dependent encoders)
*)
type ('ctx, 'a) t = {
  decode : 'a Decoder.t;
  encode : ('ctx, 'a) Encoder.t;
}

let ( let+ ) x f = Option.map f x
let ( let* ) = Option.bind

let pure (type ctx a) (x : a) : (ctx, a) t = {
  decode = Decoder.pure x;
  encode = Encoder.pure x;
}

let map (type ctx a b) (f : a -> b) (x : (ctx, a) t) : (ctx, b) t = {
  decode = Decoder.map f x.decode;
  encode = Encoder.map f x.encode;
}

let both (type ctx a b) (x : (ctx, a) t) (y : (ctx, b) t) : (ctx, a * b) t = {
  decode = Decoder.both x.decode y.decode;
  encode = Encoder.both x.encode y.encode;
}

let apply (type ctx a b) (f : (ctx, a -> b) t) (x : (ctx, a) t) : (ctx, b) t =
  both f x |> map (fun (f, x) -> f x)

let bind (type ctx a b) (x : (ctx, a) t) (y : a -> (ctx, b) t) : (ctx, b) t = {
  decode = Decoder.bind x.decode (fun x -> (y x).decode);
  encode = Encoder.bind x.encode (fun x -> (y x).encode);
}

let fail (type ctx a) : (ctx, a) t = {
  decode = Decoder.fail;
  encode = Encoder.fail;
}

let comap (type ctx ctx' a) (f : ctx' -> ctx) (x : (ctx, a) t) : (ctx', a) t =
  { x with encode = Encoder.comap f x.encode }

module Syntax = struct

  let ( <$> ) = map
  let ( <*> ) = apply

  let ( @= ) = comap

  let ( let+ ) x f = map f x
  let ( and+ ) = both
  let ( let* ) = bind
  let ( and* ) = both

end

open Sized_numbers

(* TODO: Unsigned formats *)

let int8 : (int, int) t = (* FIXME: int8 *)
  let decode buf pos = if pos < Bytes.length buf then Some (Bytes.get_int8 buf pos, pos + 1) else None
  and encode buf c = Buffer.add_int8 buf c; Some c in
  { decode; encode }

let int16_be : (int, int) t = (* FIXME: int16 *)
  let open Syntax in

  let+ b0 = (fun x -> x lsl 8) <$> (fun x -> x lsr 8) @= int8
  and+ b1 = int8 in

  b0 lor b1

let int16_le : (int, int) t = (* FIXME: int16 *)
  let open Syntax in

  let+ b0 = int8
  and+ b1 = (fun x -> x lsl 8) <$> (fun x -> x lsr 8) @= int8 in

  b0 lor b1

let int32_be : (int32, int32) t =
  let open Syntax in
  let open Int32.O in

  let+ b0 = Int32.(fun x -> (of_int x) lsl 24) <$> Int32.(fun x -> to_int (x lsr 24)) @= int8
  and+ b1 = Int32.(fun x -> (of_int x) lsl 16) <$> Int32.(fun x -> to_int (x lsr 16)) @= int8
  and+ b2 = Int32.(fun x -> (of_int x) lsl 8) <$> Int32.(fun x -> to_int (x lsr 8)) @= int8
  and+ b3 = Int32.of_int <$> Int32.to_int @= int8 in

  b0 lor b1 lor b2 lor b3

let int32_le : (int32, int32) t =
  let open Syntax in
  let open Int32.O in

  let+ b0 = Int32.of_int <$> Int32.to_int @= int8
  and+ b1 = Int32.(fun x -> (of_int x) lsl 8) <$> Int32.(fun x -> to_int (x lsr 8)) @= int8
  and+ b2 = Int32.(fun x -> (of_int x) lsl 16) <$> Int32.(fun x -> to_int (x lsr 16)) @= int8
  and+ b3 = Int32.(fun x -> (of_int x) lsl 24) <$> Int32.(fun x -> to_int (x lsr 24)) @= int8 in

  b0 lor b1 lor b2 lor b3

let int64_be : (int64, int64) t =
  let open Syntax in
  let open Int64.O in

  let+ b0 = Int64.(fun x -> (of_int x) lsl 56) <$> Int64.(fun x -> to_int (x lsr 56)) @= int8
  and+ b1 = Int64.(fun x -> (of_int x) lsl 48) <$> Int64.(fun x -> to_int (x lsr 48)) @= int8
  and+ b2 = Int64.(fun x -> (of_int x) lsl 40) <$> Int64.(fun x -> to_int (x lsr 40)) @= int8
  and+ b3 = Int64.(fun x -> (of_int x) lsl 32) <$> Int64.(fun x -> to_int (x lsr 32)) @= int8
  and+ b4 = Int64.(fun x -> (of_int x) lsl 24) <$> Int64.(fun x -> to_int (x lsr 24)) @= int8
  and+ b5 = Int64.(fun x -> (of_int x) lsl 16) <$> Int64.(fun x -> to_int (x lsr 16)) @= int8
  and+ b6 = Int64.(fun x -> (of_int x) lsl 8) <$> Int64.(fun x -> to_int (x lsr 8)) @= int8
  and+ b7 = Int64.of_int <$> Int64.to_int @= int8 in

  b0 lor b1 lor b2 lor b3 lor b4 lor b5 lor b6 lor b7

let int64_le : (int64, int64) t =
  let open Syntax in
  let open Int64.O in

  let+ b0 = Int64.of_int <$> Int64.to_int @= int8
  and+ b1 = Int64.(fun x -> (of_int x) lsl 8) <$> Int64.(fun x -> to_int (x lsr 8)) @= int8
  and+ b2 = Int64.(fun x -> (of_int x) lsl 16) <$> Int64.(fun x -> to_int (x lsr 16)) @= int8
  and+ b3 = Int64.(fun x -> (of_int x) lsl 24) <$> Int64.(fun x -> to_int (x lsr 24)) @= int8
  and+ b4 = Int64.(fun x -> (of_int x) lsl 32) <$> Int64.(fun x -> to_int (x lsr 32)) @= int8
  and+ b5 = Int64.(fun x -> (of_int x) lsl 40) <$> Int64.(fun x -> to_int (x lsr 40)) @= int8
  and+ b6 = Int64.(fun x -> (of_int x) lsl 48) <$> Int64.(fun x -> to_int (x lsr 48)) @= int8
  and+ b7 = Int64.(fun x -> (of_int x) lsl 56) <$> Int64.(fun x -> to_int (x lsr 56)) @= int8 in

  b0 lor b1 lor b2 lor b3 lor b4 lor b5 lor b6 lor b7


module List = struct

  open Syntax

  let repeat_len (type a) (elem : (a, a) t) (len : int) : (a list, a list) t =
    let rec repeat_len i =
      if i < len then
        (* FIXME: List.hd and List.tl fail on the empty list*)
        let* x = List.hd @= elem in
        let+ xs = List.tl @= repeat_len (i + 1) in
        x :: xs
      else
        pure []
    in
    repeat_len 0

end

module Array = struct

  let repeat_len (type a) (elem : (a, a) t) (len : int) : (a array, a array) t =
    (* FIXME: No clue if this actually works - write some tests! *)
    let decode buf pos =
      if len <= 0 then Some ([||], pos) else
        let* (x, pos) = elem.decode buf pos in
        let xs = Array.make len x in
        let rec go i buf pos =
          if i < len then
            let* (x, pos) = elem.decode buf pos in
            Array.set xs i x;
            (go [@tailcall]) (i + 1) buf pos
          else
            Some (xs, pos)
        in
        go 1 buf pos

    and encode =
      let rec go i buf xs =
        if i <= Array.length xs then
          let* _ = elem.encode buf (Array.get xs i) in
          (go [@tailcall]) (i + 1) buf xs
        else
          Some xs
      in
      go 0
    in

    { decode; encode }

end
