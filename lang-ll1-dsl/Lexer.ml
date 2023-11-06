open Parser

exception UnexpectedChar
exception UnclosedBlockComment

let dec_digit = [%sedlex.regexp? '0'..'9']
let hex_digit = [%sedlex.regexp? '0'..'9' | 'A'..'F' | 'a'..'f']

let dec_number = [%sedlex.regexp? Plus dec_digit]
let hex_number = [%sedlex.regexp? "0x", Plus hex_digit]

let name_start = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let name_continue = [%sedlex.regexp? '-' | '_' | 'a'..'z' | 'A'..'Z' | '0'..'9']
let name = [%sedlex.regexp? name_start, Star name_continue]

let rec block_comment lexbuf level =
  match%sedlex lexbuf with
  | "/-" -> (block_comment [@tailrec]) lexbuf (level + 1)
  | "-/" -> if level = 0 then () else (block_comment [@tailrec]) lexbuf (level - 1)
  | any -> (block_comment [@tailrec]) lexbuf level
  | eof -> raise UnclosedBlockComment
  | _ -> raise UnexpectedChar

let rec token lexbuf =
  match%sedlex lexbuf with
  | ' ' | '\t' | '\n' -> (token [@tailrec]) lexbuf
  | "--", Star (Compl '\n'), '\n' -> (token [@tailrec]) lexbuf
  | "/-" -> block_comment lexbuf 0; (token [@tailrec]) lexbuf
  | dec_number -> INT (int_of_string (Sedlexing.Latin1.lexeme lexbuf))
  | hex_number -> INT (int_of_string (Sedlexing.Latin1.lexeme lexbuf))
  | "def" -> KEWORD_DEF
  | name -> NAME (Sedlexing.Latin1.lexeme lexbuf)
  | "!" -> BANG
  | ":" -> COLON
  | ":=" -> COLON_EQUALS
  | "," -> COMMA
  | "=>" -> EQUALS_GREATER
  | "." -> FULL_STOP
  | "<-" -> LESS_HYPHEN
  | "|" -> PIPE
  | ";" -> SEMI
  | ".." -> DOT_DOT
  | "..<" -> DOT_DOT_LESS
  | ">.." -> GREATER_DOT_DOT
  | ">..<" -> GREATER_DOT_DOT_LESS
  | '{' -> LBRACE
  | '}' -> RBRACE
  | '(' -> LPAREN
  | ')' -> RPAREN
  | eof -> END
  | _ -> raise UnexpectedChar
