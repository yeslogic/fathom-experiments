module SExpr = struct

  open Ll1.Parser
  open Ll1.ParserUtil

  let string_of_list cs =
    let buf = Buffer.create 0 in
    List.iter (Buffer.add_char buf) cs;
    Buffer.contents buf

  let letter = byte_set Byte_set.(union (range 'a' 'z') (range 'A' 'Z'))
  let digit = byte_set Byte_set.(range '0' '9')

  let space = byte_set Byte_set.(of_string " \t\n")
  let spaces0 = skip_many0 space
  let spaces1 = skip_many1 space

  let symbol =
    let+ c = letter
    and+ cs = many0 (one_of [letter; digit]) in
    string_of_list (c :: cs)

  let paren p =
    let+ () = byte '('
    and+ expr = p
    and+ () = byte ')' in
    expr

  type t =
    | Sym of string
    | Seq of t list

  let sexp =
    (* spaces0 *> *) (* FIXME: Space before *)
    fix (fun expr -> one_of [
      symbol ==> (fun s -> Sym s);
      paren (sep0 expr spaces1) ==> (fun xs -> Seq xs);
      (* FIXME:        ^^^^^^^ Empty spaces before/after seqences *)
      (* FIXME: ^^^^^^^^^^^^^^ Spaces between parens *)
    ])
    <* spaces0

end

open SExpr

(* This module randomly generates some huge s-expressions, and
    then tries to parse them *)

let generate_symbol _fuel =
  Char.(escaped (chr (65 + Random.int 26)))

let rec generate_list g (fuel : int) =
  match fuel with
  | 0 -> []
  | 1 -> [g fuel]
  | fuel ->
      let i = Random.int fuel in (* Divide the fuel *)
      let x = g i in
      let xs = generate_list g (fuel - i) in
      x :: xs

let rec generate_sexp fuel =
  match fuel with
  | 0 -> Seq []
  | 1 -> Sym (generate_symbol fuel)
  | fuel -> Seq (generate_list generate_sexp fuel)

let rec pp_print_sexp ppf = function
  | Sym s -> Format.fprintf ppf "%s" s
  | Seq xs -> Format.fprintf ppf "(%a)" pp_print_sexps xs
and pp_print_sexps ppf = function
  | [] -> ()
  | [s] -> pp_print_sexp ppf s
  | x :: xs -> Format.fprintf ppf "%a %a" pp_print_sexp x pp_print_sexps xs

let string_of_sexp sexp =
  let b = Buffer.create 0 in
  let ppf = Format.formatter_of_buffer b in
  let () = pp_print_sexp ppf sexp in
  Buffer.to_bytes b


let time f x =
  Printexc.record_backtrace true;
  let t = Sys.time () in
  let _ = f x in
  Sys.time () -. t

let s100 () = string_of_sexp (generate_sexp 100)
(* let s1000k () = string_of_sexp (generate_sexp 1000000) *)
(* let s10M () = string_of_sexp (generate_sexp 10000000) *)

let test str =
  let len = Bytes.length str in
  let t = str |> time (fun s -> Ll1.Parser.parse sexp s 0) in
  let rate = (float_of_int len /. t) in begin
    Printf.printf "String length: %d bytes\n" len;
    Printf.printf "Parser elapsed time: %.3f sec\n" t;
    Printf.printf "Parsing rate: %.3g bytes/sec\n\n" rate;
  end

let _ = test (s100 ())
(* let _ = test (s1000k ()) *)
(* let _ = test (s10M ()) *)
