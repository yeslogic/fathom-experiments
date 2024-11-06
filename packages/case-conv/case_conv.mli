(** Case transformation functions *)

(** {1 Split strings into words} *)

val from_camel : string -> string list
val from_words : string -> string list
val from_kebab : string -> string list
val from_snake : string -> string list
val from_any : string -> string list

(** {1 Recapitalise words into strings} *)

val to_pascal_case : string list -> string
val to_camel_case : string list -> string
val to_quiet_kebab_case : string list -> string
val to_screaming_kebab_case : string list -> string
val to_pascal_kebab_case : string list -> string
val to_capitalised_kebab_case : string list -> string
val to_quiet_snake_case : string list -> string
val to_screaming_snake_case : string list -> string
val to_pascal_snake_case : string list -> string
val to_capitalised_snake_case : string list -> string
val to_sentence_case : string list -> string
val to_title_case : string list -> string
val to_lower_sentence_case : string list -> string
val to_upper_sentence_case : string list -> string

(** {1 Arbirary string recapitalisation} *)

val pascal_case : string -> string
val camel_case : string -> string
val quiet_kebab_case : string -> string
val screaming_kebab_case : string -> string
val pascal_kebab_case : string -> string
val capitalised_kebab_case : string -> string
val quiet_snake_case : string -> string
val screaming_snake_case : string -> string
val pascal_snake_case : string -> string
val capitalised_snake_case : string -> string
val sentence_case : string -> string
val title_case : string -> string
val lower_sentence_case : string -> string
val upper_sentence_case : string -> string
