open Parser

exception Unexpected_char
exception Unclosed_block_comment

let dec_digit = [%sedlex.regexp? '0'..'9']
let hex_digit = [%sedlex.regexp? '0'..'9' | 'A'..'F' | 'a'..'f']

let dec_number = [%sedlex.regexp? Plus dec_digit]
let hex_number = [%sedlex.regexp? "0x", Plus hex_digit]

let name_start = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let name_continue = [%sedlex.regexp? '-' | '_' | 'a'..'z' | 'A'..'Z' | '0'..'9']
let name = [%sedlex.regexp? name_start, Star name_continue]

let rec block_comment lexbuf level =
  match%sedlex lexbuf with
  | "/-" -> (block_comment [@tailcall]) lexbuf (level + 1)
  | "-/" -> if level = 0 then () else (block_comment [@tailcall]) lexbuf (level - 1)
  | any -> (block_comment [@tailcall]) lexbuf level
  | eof -> raise Unclosed_block_comment
  | _ -> raise Unexpected_char

let rec token lexbuf =
  match%sedlex lexbuf with
  | ' ' | '\t' | '\n' -> (token [@tailcall]) lexbuf
  | "--", Star (Compl '\n'), '\n' -> (token [@tailcall]) lexbuf
  | "/-" -> block_comment lexbuf 0; (token [@tailcall]) lexbuf

  | dec_number -> INT (Sedlexing.Latin1.lexeme lexbuf)
  | hex_number -> INT (Sedlexing.Latin1.lexeme lexbuf)

  | "def" -> KEYWORD_DEF
  | "else" -> KEYWORD_ELSE
  | "format" -> KEYWORD_FORMAT
  | "if" -> KEYWORD_IF
  | "let" -> KEYWORD_LET
  | "then" -> KEYWORD_THEN
  | "type" -> KEYWORD_TYPE

  | name -> NAME (Sedlexing.Latin1.lexeme lexbuf)

  | "&" -> AMPERSAND
  | "*" -> ASTERISK
  | "!" -> BANG
  | "!=" -> BANG_EQUALS
  | "^" -> CARET
  | ":" -> COLON
  | ":=" -> COLON_EQUALS
  | "=" -> EQUALS
  | ">" -> GREATER
  | ">=" -> GREATER_EQUAL
  | ">>" -> GREATER_GREATER
  (* | "," -> COMMA *)
  | "/" -> FORWARD_SLASH
  | "." -> FULL_STOP
  | "-" -> HYPHEN
  | "<-" -> LESS_HYPHEN
  | "<" -> LESS
  | "<=" -> LESS_EQUAL
  | "<<" -> LESS_LESS
  | "|" -> PIPE
  | "+" -> PLUS
  | ";" -> SEMI

  | '{' -> LBRACE
  | '}' -> RBRACE
  | '(' -> LPAREN
  | ')' -> RPAREN

  | eof -> END
  | _ -> raise Unexpected_char
