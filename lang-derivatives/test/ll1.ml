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
  let syntax = alts [
    char_of "A";
    char_of "B";
  ] in

  let parser = parse syntax |> Option.get in

  assert (parser (String.to_seq "A") = Some 'A');
  assert (parser (String.to_seq "B") = Some 'B');
  assert (parser (String.to_seq "C") = None);
  assert (parser (String.to_seq "AB") = None)

(* nullable left *)
let () =
  let syntax = alts [
    pure 'A' |> map Fun.id;
    char 'B';
  ] in

  let parser = parse syntax |> Option.get in

  assert (parser (String.to_seq "") = Some 'A');
  assert (parser (String.to_seq "B") = Some 'B');
  assert (parser (String.to_seq "C") = None)

(* nullable right *)
let () =
  let syntax = alts [
    char 'A';
    pure 'B' |> map Fun.id;
  ] in

  let parser = parse syntax |> Option.get in

  assert (parser (String.to_seq "A") = Some 'A');
  assert (parser (String.to_seq "") = Some 'B');
  assert (parser (String.to_seq "C") = None)

(* nullable both *)
let () =
  let syntax = alts [
    alt (pure 'B') (char 'A');
    pure 'B';
  ] in

  assert (Option.is_none (parse syntax))

let () =
  let syntax = alts [
    cat [pure (); char 'B'];
    char 'A' |> map (fun x -> [(); x]);
  ] in

  let parser = parse syntax |> Option.get in

  assert (parser (String.to_seq "A") = Some [(); 'A']);
  assert (parser (String.to_seq "B") = Some [(); 'B']);
  assert (parser (String.to_seq "C") = None);
  assert (parser (String.to_seq "AB") = None)

(* Ambiguous alternations *)

let () =
  assert (Option.is_none (parse (alts [char 'A'; char 'A'])))

let () =
  assert (Option.is_none (parse (alts [
    cat [pure (); char 'A'];
    char 'A' |> map (fun x -> [(); x]);
  ])))

let () =
  assert (Option.is_none (parse (alts [
    cat [pure (); char 'A'; char 'B'];
    cat [char 'A'; char 'C'] |> map (fun xs -> () :: xs);
  ])))
