open Derivatives.Syntax.Peg_ll1.Char

(** Create a parser that asserts that both the derivative-based and compiled
    parsers produce the same result. *)
let make_parser s =
  let det_s = compile s in
  fun ts ->
    let ts = String.to_seq ts in
    let x = parse s ts in
    let y = Case_tree.parse det_s ts in
    assert (x = y);
    x

let () =
  Printexc.record_backtrace true

let () =
  let parser = make_parser Notation.(char 'A' <|> char 'B') in

  assert (parser "A" = Some 'A');
  assert (parser "B" = Some 'B');
  assert (parser "C" = None);
  assert (parser "AB" = None)

(* nullable left *)
let () =
  let parser = make_parser Notation.(pure 'A' <|> char 'B') in

  assert (parser "" = Some 'A');
  assert (parser "B" = Some 'B');
  assert (parser "C" = None)

(* nullable right *)
let () =
  let parser = make_parser Notation.(
    char 'A' <|> pure 'B'
  ) in

  assert (parser "A" = Some 'A');
  assert (parser "" = Some 'B');
  assert (parser "C" = None)

(* nullable both *)
let () =
  match Notation.(
    (pure 'B' <|> char 'A') <|> pure 'B'
  ) with
  | exception Nullable_conflict -> ()
  | _ -> failwith "expected Nullable_conflict"

let () =
  let parser = make_parser Notation.(
    pure () +> char 'B' <|> char 'A'
  ) in

  assert (parser "A" = Some 'A');
  assert (parser "B" = Some 'B');
  assert (parser "C" = None);
  assert (parser "AB" = None)

(* Ambiguous alternations *)

let () =
  match Notation.(char 'A' <|> char 'A') with
  | exception First_conflict -> ()
  | _ -> failwith "expected First_conflict"

let () =
  match Notation.(pure () +> char 'A' <|> char 'A') with
  | exception First_conflict -> ()
  | _ -> failwith "expected First_conflict"

let () =
  match Notation.(char 'A' <|> pure () +> char 'A') with
  | exception First_conflict -> ()
  | _ -> failwith "expected First_conflict"

let () =
  match Notation.(pure () +> string "AB" <|> string "AC") with
  | exception First_conflict -> ()
  | _ -> failwith "expected First_conflict"
