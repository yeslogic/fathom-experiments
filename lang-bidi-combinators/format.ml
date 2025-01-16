type ('c, 'a) t = {
  decode : 'a Decoder.t;
  encode : ('c, 'a) Encoder.t;
}

type 'a value = ('a, 'a) t

let pure (type a c) (x : a) : (c, a) t = {
  decode = Decoder.pure x;
  encode = Encoder.pure x;
}

let map (type a b c) (f : a -> b) (x : (c, a) t) : (c, b) t = {
  decode = Decoder.map f x.decode;
  encode = Encoder.map f x.encode;
}

let both (type a b c) (x : (c, a) t) (y : (c, b) t) : (c, a * b) t = {
  decode = Decoder.both x.decode y.decode;
  encode = Encoder.both x.encode y.encode;
}

let apply (type a b c) (f : (c, a -> b) t) (x : (c, a) t) : (c, b) t =
  both f x |> map (fun (f, x) -> f x)

let bind (type a b c) (x : (c, a) t) (y : a -> (c, b) t) : (c, b) t = {
  decode = Decoder.bind x.decode (fun x -> (y x).decode);
  encode = Encoder.bind x.encode (fun x -> (y x).encode);
}

let fail (type a c) : (c, a) t = {
  decode = Decoder.fail;
  encode = Encoder.fail;
}

let alt (type a c) (x : (c, a) t) (y : (c, a) t) : (c, a) t = {
  decode = Decoder.alt x.decode y.decode;
  encode = Encoder.alt x.encode y.encode;
}

let dimap (type a b c d) (f : a -> b) (g : d -> c) (x : (c, a) t) : (d, b) t = {
  decode = Decoder.map f x.decode;
  encode = Encoder.dimap f g x.encode;
}

let comap (type a c d) (f : d -> c) (x : (c, a) t) : (d, a) t =
  { x with encode = Encoder.comap f x.encode }


let const (type a c) (x : a value) (expected : a) : (c, unit) t = {
  decode = Decoder.const x.decode expected;
  encode = Encoder.const x.encode expected;
}

let unused (type a c) (x : a value) ~(default : a) : (c, unit) t =
  x |> dimap (fun _ -> ()) (fun _ -> default)

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

open Sized_numbers

(* TODO: Unsigned formats *)


(* Integer conversion formats *)

let int_to_int32 : int value -> int32 value =
  dimap Int32.of_int Int32.to_int_trunc

let int_to_int64 : int value -> int64 value =
  dimap Int64.of_int Int64.to_int_trunc


(* Integer formats *)

(** FIXME: these formats reconstruct the same integer during encoding, rather
    than just returning the input value unchanged. *)

let int8 : int value = (* FIXME: int8 *)
  let decode buf pos = if pos < Bytes.length buf then Some (Bytes.get_int8 buf pos, pos + 1) else None
  and encode buf c = Buffer.add_int8 buf c; Some c in
  { decode; encode }

let int16_be : int value = (* FIXME: int16 *)
  let open Syntax in

  let+ b0 = int8 |> dimap (fun x -> x lsl 8) (fun x -> x lsr 8)
  and+ b1 = int8 in

  b0 lor b1

let int16_le : int value = (* FIXME: int16 *)
  let open Syntax in

  let+ b0 = int8
  and+ b1 = int8 |> dimap (fun x -> x lsl 8) (fun x -> x lsr 8) in

  b0 lor b1

let int32_be : int32 value =
  let open Syntax in
  let open Int32.O in

  let+ b0 = int_to_int32 int8 |> dimap (fun x -> x lsl 24) (fun x -> x lsr 24)
  and+ b1 = int_to_int32 int8 |> dimap (fun x -> x lsl 16) (fun x -> x lsr 16)
  and+ b2 = int_to_int32 int8 |> dimap (fun x -> x lsl 8) (fun x -> x lsr 8)
  and+ b3 = int_to_int32 int8 in

  b0 lor b1 lor b2 lor b3

let int32_le : int32 value =
  let open Syntax in
  let open Int32.O in

  let+ b0 = int_to_int32 int8
  and+ b1 = int_to_int32 int8 |> dimap (fun x -> x lsl 8) (fun x -> x lsr 8)
  and+ b2 = int_to_int32 int8 |> dimap (fun x -> x lsl 16) (fun x -> x lsr 16)
  and+ b3 = int_to_int32 int8 |> dimap (fun x -> x lsl 24) (fun x -> x lsr 24) in

  b0 lor b1 lor b2 lor b3

let int64_be : int64 value =
  let open Syntax in
  let open Int64.O in

  let+ b0 = int_to_int64 int8 |> dimap (fun x -> x lsl 56) (fun x -> x lsr 56)
  and+ b1 = int_to_int64 int8 |> dimap (fun x -> x lsl 48) (fun x -> x lsr 48)
  and+ b2 = int_to_int64 int8 |> dimap (fun x -> x lsl 40) (fun x -> x lsr 40)
  and+ b3 = int_to_int64 int8 |> dimap (fun x -> x lsl 32) (fun x -> x lsr 32)
  and+ b4 = int_to_int64 int8 |> dimap (fun x -> x lsl 24) (fun x -> x lsr 24)
  and+ b5 = int_to_int64 int8 |> dimap (fun x -> x lsl 16) (fun x -> x lsr 16)
  and+ b6 = int_to_int64 int8 |> dimap (fun x -> x lsl 8) (fun x -> x lsr 8)
  and+ b7 = int_to_int64 int8 in

  b0 lor b1 lor b2 lor b3 lor b4 lor b5 lor b6 lor b7

let int64_le : int64 value =
  let open Syntax in
  let open Int64.O in

  let+ b0 = int_to_int64 int8
  and+ b1 = int_to_int64 int8 |> dimap (fun x -> x lsl 8) (fun x -> x lsr 8)
  and+ b2 = int_to_int64 int8 |> dimap (fun x -> x lsl 16) (fun x -> x lsr 16)
  and+ b3 = int_to_int64 int8 |> dimap (fun x -> x lsl 24) (fun x -> x lsr 24)
  and+ b4 = int_to_int64 int8 |> dimap (fun x -> x lsl 32) (fun x -> x lsr 32)
  and+ b5 = int_to_int64 int8 |> dimap (fun x -> x lsl 40) (fun x -> x lsr 40)
  and+ b6 = int_to_int64 int8 |> dimap (fun x -> x lsl 48) (fun x -> x lsr 48)
  and+ b7 = int_to_int64 int8 |> dimap (fun x -> x lsl 56) (fun x -> x lsr 56) in

  b0 lor b1 lor b2 lor b3 lor b4 lor b5 lor b6 lor b7


module List = struct

  open Syntax

  let repeat_len (type a) (elem : a value) (len : int) : a list value =
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

  let ( let* ) = Option.bind

  let repeat_len (type a) (elem : a value) (len : int) : a array value =
    (* FIXME: No clue if this actually works... and it’s really ugly! write some tests! *)
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
