(*
  Inspired by https://hackage.haskell.org/package/casing

  Most likely sloooow - the aim is clarity (for now).
*)

let ( >> ) (type a b c) (f : a -> b) (g : b -> c) : a -> c =
  fun x -> g (f x)

let from_camel (s : string) : string list = begin
  let words = ref [] in
  let state = ref `Upper in
  let start = ref 0 in

  begin s |> String.iteri @@ fun i c ->
    match !state, c with
    | `Other, 'A'..'Z' ->
        words := String.sub s !start (i - !start) :: !words;
        state := `Upper;
        start := i;
    | `Upper, 'A'..'Z' -> ()
    | _, _ ->
        state := `Other;
  end;

  let last_len = String.length s - !start in
  if last_len > 0 then
    words := String.sub s !start last_len :: !words;

  List.rev !words
end

(* let () = begin
  assert (from_camel "" = []);
  assert (from_camel "fooBar" = ["foo"; "Bar"]);
  assert (from_camel "FooBar" = ["Foo"; "Bar"]);
  assert (from_camel "HTMLfoo" = ["HTMLfoo"]);
  assert (from_camel "FooABCyeah123_Hi" = ["Foo"; "ABCyeah123_"; "Hi"]);
end *)

let from_words : string -> string list = String.split_on_char ' ' >> List.filter (( != ) "")
let from_kebab : string -> string list = String.split_on_char '-' >> List.filter (( != ) "")
let from_snake : string -> string list = String.split_on_char '-' >> List.filter (( != ) "")

let from_any (s : string) : string list =
  from_camel s
  |> List.concat_map from_words
  |> List.concat_map from_kebab
  |> List.concat_map from_snake

let head_tail (type a) (head : a) (tail : a) : int -> a =
  function
  | 0 -> head
  | _ -> tail

let lower       : string -> string = String.lowercase_ascii
let upper       : string -> string = String.uppercase_ascii
let capitalised : string -> string = String.lowercase_ascii >> String.capitalize_ascii

let smooshify   : string list -> string = String.concat ""
let kebabify    : string list -> string = String.concat "-"
let snakeify    : string list -> string = String.concat "_"
let sentenceify : string list -> string = String.concat " "

let to_pascal_case            : string list -> string = List.map capitalised >> smooshify
let to_camel_case             : string list -> string = List.mapi (head_tail lower capitalised) >> smooshify
let to_quiet_kebab_case       : string list -> string = List.map lower >> kebabify
let to_screaming_kebab_case   : string list -> string = List.map upper >> kebabify
let to_pascal_kebab_case      : string list -> string = List.map capitalised >> kebabify
let to_capitalised_kebab_case : string list -> string = List.mapi (head_tail capitalised lower) >> kebabify
let to_quiet_snake_case       : string list -> string = List.map lower >> snakeify
let to_screaming_snake_case   : string list -> string = List.map upper >> snakeify
let to_pascal_snake_case      : string list -> string = List.map capitalised >> snakeify
let to_capitalised_snake_case : string list -> string = List.mapi (head_tail capitalised lower) >> snakeify
let to_sentence_case          : string list -> string = List.mapi (head_tail capitalised lower) >> sentenceify
let to_title_case             : string list -> string = List.map capitalised >> sentenceify
let to_lower_sentence_case    : string list -> string = List.map lower >> sentenceify
let to_upper_sentence_case    : string list -> string = List.map upper >> sentenceify

let pascal_case               : string -> string = from_any >> to_pascal_case
let camel_case                : string -> string = from_any >> to_camel_case
let quiet_kebab_case          : string -> string = from_any >> to_quiet_kebab_case
let screaming_kebab_case      : string -> string = from_any >> to_screaming_kebab_case
let pascal_kebab_case         : string -> string = from_any >> to_pascal_kebab_case
let capitalised_kebab_case    : string -> string = from_any >> to_capitalised_kebab_case
let quiet_snake_case          : string -> string = from_any >> to_quiet_snake_case
let screaming_snake_case      : string -> string = from_any >> to_screaming_snake_case
let pascal_snake_case         : string -> string = from_any >> to_pascal_snake_case
let capitalised_snake_case    : string -> string = from_any >> to_capitalised_snake_case
let sentence_case             : string -> string = from_any >> to_sentence_case
let title_case                : string -> string = from_any >> to_title_case
let lower_sentence_case       : string -> string = from_any >> to_lower_sentence_case
let upper_sentence_case       : string -> string = from_any >> to_upper_sentence_case
