(** A binary format similar to the example found in {{:https://doi.org/10.1145/3341686}
    “Narcissus: correct-by-construction derivation of decoders and encoders from
    binary formats”}
*)

open Bidi_combinators

type t = {
  station_id : int;
  data : int array;
}

let station_id x = x.station_id
let data x = x.data

let ( >> ) f g x = f x |> g

let format : t Format.value =
  let open Format.Syntax in

  let* station_id = station_id @= Format.int8
  and* length = (data >> Array.length) @= Format.int8 in
  let+ data = data @= Format.Array.repeat_len Format.int8 length in

  { station_id; data }
