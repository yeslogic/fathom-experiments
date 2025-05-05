open Derivatives.Syntax.Ll1_simple.Char

type _ cat =
  | [] : unit cat
  | ( :: ) : 'hd syntax * 'tl cat -> ('hd * 'tl) cat

type _ tuple =
  | [] : unit tuple
  | ( :: ) : 'hd * 'tl tuple -> ('hd * 'tl) tuple

let char ch = elem (Byte_set.singleton ch)
let char_of s = elem (Byte_set.of_string s)

let rec alts : 'a syntax list -> 'a syntax =
  function
  | [] -> invalid_arg "alts"
  | [s] -> s
  | s :: ss -> alt s (alts ss)

let rec cat : type a. a cat -> a tuple syntax =
  function
  | [] -> pure []
  | s :: ss -> seq s (cat ss) |> map (fun (x, xs) -> x :: xs)

let parse_both s ts =
  let ts = String.to_seq ts in
  let x = parse s ts in
  let y = Det.parse (compile s) ts in
  assert (x = y);
  x

let () =
  Printexc.record_backtrace true

let () =
  let syntax = alts [
    char_of "A";
    char_of "B";
  ] in

  assert (parse_both syntax "A" = Some 'A');
  assert (parse_both syntax "B" = Some 'B');
  assert (parse_both syntax "C" = None);
  assert (parse_both syntax "AB" = None)

(* nullable left *)
let () =
  let syntax = alts [
    pure 'A' |> map Fun.id;
    char 'B';
  ] in

  assert (parse_both syntax "" = Some 'A');
  assert (parse_both syntax "B" = Some 'B');
  assert (parse_both syntax "C" = None)

(* nullable right *)
let () =
  let syntax = alts [
    char 'A';
    pure 'B' |> map Fun.id;
  ] in

  assert (parse_both syntax "A" = Some 'A');
  assert (parse_both syntax "" = Some 'B');
  assert (parse_both syntax "C" = None)

(* nullable both *)
let () =
  match alts [
    alt (pure 'B') (char 'A');
    pure 'B';
  ] with
  | exception Nullable_conflict -> ()
  | _ -> failwith "expected Nullable_conflict"

let () =
  let syntax = alts [
    cat [pure (); char 'B'];
    char 'A' |> map (fun x -> [(); x]);
  ] in

  assert (parse_both syntax "A" = Some [(); 'A']);
  assert (parse_both syntax "B" = Some [(); 'B']);
  assert (parse_both syntax "C" = None);
  assert (parse_both syntax "AB" = None)

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
