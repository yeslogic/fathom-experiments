open Derivatives.Syntax.Ll1_derive.Char

type _ cat =
  | [] : unit cat
  | ( :: ) : 'hd t * 'tl cat -> ('hd * 'tl) cat

type _ tuple =
  | [] : unit tuple
  | ( :: ) : 'hd * 'tl tuple -> ('hd * 'tl) tuple

let char ch = elem (Byte_set.singleton ch)
let char_of s = elem (Byte_set.of_string s)

let rec alts : 'a t list -> 'a t =
  function
  | [] -> invalid_arg "alts"
  | [s] -> s
  | s :: ss -> alt s (alts ss)

let rec cat : type a. a cat -> a tuple t =
  function
  | [] -> pure []
  | s :: ss -> seq s (cat ss) |> map (fun (x, xs) -> x :: xs)

(** Create a parser that asserts that both the derivative-based and compiled
    parsers produce the same result. *)
let make_parser s =
  let det_s = compile s in
  fun ts ->
    let ts = String.to_seq ts in
    let x = parse s ts in
    let y = Det.parse det_s ts in
    assert (x = y);
    x

let () =
  Printexc.record_backtrace true

let () =
  let parser = make_parser @@ alts [
    char_of "A";
    char_of "B";
  ] in

  assert (parser "A" = Some 'A');
  assert (parser "B" = Some 'B');
  assert (parser "C" = None);
  assert (parser "AB" = None)

(* nullable left *)
let () =
  let parser = make_parser @@ alts [
    pure 'A' |> map Fun.id;
    char 'B';
  ] in

  assert (parser "" = Some 'A');
  assert (parser "B" = Some 'B');
  assert (parser "C" = None)

(* nullable right *)
let () =
  let parser = make_parser @@ alts [
    char 'A';
    pure 'B' |> map Fun.id;
  ] in

  assert (parser "A" = Some 'A');
  assert (parser "" = Some 'B');
  assert (parser "C" = None)

(* nullable both *)
let () =
  match alts [
    alt (pure 'B') (char 'A');
    pure 'B';
  ] with
  | exception Nullable_conflict -> ()
  | _ -> failwith "expected Nullable_conflict"

let () =
  let parser = make_parser @@ alts [
    cat [pure (); char 'B'];
    char 'A' |> map (fun x -> [(); x]);
  ] in

  assert (parser "A" = Some [(); 'A']);
  assert (parser "B" = Some [(); 'B']);
  assert (parser "C" = None);
  assert (parser "AB" = None)

(* Ambiguous alternations *)

let () =
  match alts [char 'A'; char 'A'] with
  | exception First_conflict -> ()
  | _ -> failwith "expected First_conflict"

let () =
  match alts [
    cat [pure (); char 'A'];
    char 'A' |> map (fun x -> [(); x]);
  ] with
  | exception First_conflict -> ()
  | _ -> failwith "expected First_conflict"

let () =
  match alts [
    cat [pure (); char 'A'; char 'B'];
    cat [char 'A'; char 'C'] |> map (fun xs -> () :: xs);
  ] with
  | exception First_conflict -> ()
  | _ -> failwith "expected First_conflict"
