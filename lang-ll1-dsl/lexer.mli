exception UnexpectedChar
exception UnclosedBlockComment

val token : Sedlexing.lexbuf -> Parser.token
