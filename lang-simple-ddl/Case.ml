(*
  Inspired by https://hackage.haskell.org/package/casing

  Most likely verrrry sloooow the goal is clarity for now.
  Because I didn't find any decent libraries.
*)

let from_humps (s : string) : string list =
  (* FIXME: split on camel case boundaries *)
  [s]

let from_words (s : string) : string list = String.split_on_char ' ' s |> List.filter (( != ) "")
let from_kebab (s : string) : string list = String.split_on_char '-' s |> List.filter (( != ) "")
let from_snake (s : string) : string list = String.split_on_char '-' s |> List.filter (( != ) "")

let from_any (s : string) : string list =
  from_humps s
  |> List.concat_map from_words
  |> List.concat_map from_kebab
  |> List.concat_map from_snake

let head_tail (type a) (head : a) (tail : a) : int -> a =
  function
  | 0 -> head
  | _ -> tail

let lower : string -> string = String.lowercase_ascii
let upper : string -> string = String.uppercase_ascii
let capitalised (s : string) : string = String.capitalize_ascii (lower s)

let smooshify   : string list -> string = String.concat ""
let kebabify    : string list -> string = String.concat "-"
let snakeify    : string list -> string = String.concat "_"
let sentenceify : string list -> string = String.concat " "

let to_quiet_case             (words : string list) : string = words |> List.map lower |> smooshify
let to_screaming_case         (words : string list) : string = words |> List.map upper |> smooshify
let to_pascal_case            (words : string list) : string = words |> List.map capitalised |> smooshify
let to_camel_case             (words : string list) : string = words |> List.mapi (head_tail lower capitalised) |> smooshify
let to_kebab_case             (words : string list) : string = words |> kebabify
let to_quiet_kebab_case       (words : string list) : string = words |> List.map lower |> kebabify
let to_screaming_kebab_case   (words : string list) : string = words |> List.map upper |> kebabify
let to_pascal_kebab_case      (words : string list) : string = words |> List.map capitalised |> kebabify
let to_capitalised_kebab_case (words : string list) : string = words |> List.mapi (head_tail capitalised lower) |> kebabify
let to_snake_case             (words : string list) : string = words |> snakeify
let to_quiet_snake_case       (words : string list) : string = words |> List.map lower |> snakeify
let to_screaming_snake_case   (words : string list) : string = words |> List.map upper |> snakeify
let to_pascal_snake_case      (words : string list) : string = words |> List.map capitalised |> snakeify
let to_capitalised_snake_case (words : string list) : string = words |> List.mapi (head_tail capitalised lower) |> snakeify
let to_sentence_case          (words : string list) : string = words |> List.mapi (head_tail capitalised lower) |> sentenceify
let to_title_case             (words : string list) : string = words |> List.map capitalised |> sentenceify
let to_lower_sentence_case    (words : string list) : string = words |> List.map lower |> sentenceify
let to_upper_sentence_case    (words : string list) : string = words |> List.map upper |> sentenceify

let quiet_case                (s : string) : string = lower s
let screaming_case            (s : string) : string = upper s
let pascal_case               (s : string) : string = from_any s |> to_pascal_case
let camel_case                (s : string) : string = from_any s |> to_camel_case
let kebab_case                (s : string) : string = from_any s |> to_kebab_case
let quiet_kebab_case          (s : string) : string = from_any s |> to_quiet_kebab_case
let screaming_kebab_case      (s : string) : string = from_any s |> to_screaming_kebab_case
let pascal_kebab_case         (s : string) : string = from_any s |> to_pascal_kebab_case
let capitalised_kebab_case    (s : string) : string = from_any s |> to_capitalised_kebab_case
let snake_case                (s : string) : string = from_any s |> to_snake_case
let quiet_snake_case          (s : string) : string = from_any s |> to_quiet_snake_case
let screaming_snake_case      (s : string) : string = from_any s |> to_screaming_snake_case
let pascal_snake_case         (s : string) : string = from_any s |> to_pascal_snake_case
let capitalised_snake_case    (s : string) : string = from_any s |> to_capitalised_snake_case
let sentence_case             (s : string) : string = from_any s |> to_sentence_case
let title_case                (s : string) : string = from_any s |> to_title_case
let lower_sentence_case       (s : string) : string = from_any s |> to_lower_sentence_case
let upper_sentence_case       (s : string) : string = from_any s |> to_upper_sentence_case
