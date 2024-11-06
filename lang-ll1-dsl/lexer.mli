exception Unexpected_char
exception Unclosed_block_comment

val token : Sedlexing.lexbuf -> Parser.token
