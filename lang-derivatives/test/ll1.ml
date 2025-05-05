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

let () =
  Printexc.record_backtrace true

let () =
  let syntax = alts [
    char_of "A";
    char_of "B";
  ] in

  assert (parse syntax (String.to_seq "A") = Some 'A');
  assert (parse syntax (String.to_seq "B") = Some 'B');
  assert (parse syntax (String.to_seq "C") = None);
  assert (parse syntax (String.to_seq "AB") = None)

(* nullable left *)
let () =
  let syntax = alts [
    pure 'A' |> map Fun.id;
    char 'B';
  ] in

  assert (parse syntax (String.to_seq "") = Some 'A');
  assert (parse syntax (String.to_seq "B") = Some 'B');
  assert (parse syntax (String.to_seq "C") = None)

(* nullable right *)
let () =
  let syntax = alts [
    char 'A';
    pure 'B' |> map Fun.id;
  ] in

  assert (parse syntax (String.to_seq "A") = Some 'A');
  assert (parse syntax (String.to_seq "") = Some 'B');
  assert (parse syntax (String.to_seq "C") = None)

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

  assert (parse syntax (String.to_seq "A") = Some [(); 'A']);
  assert (parse syntax (String.to_seq "B") = Some [(); 'B']);
  assert (parse syntax (String.to_seq "C") = None);
  assert (parse syntax (String.to_seq "AB") = None)

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
