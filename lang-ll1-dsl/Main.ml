let print_error (start, _ : Lexing.position * Lexing.position) message =
  Printf.eprintf "%s:%d:%d: %s\n"
      start.pos_fname
      start.pos_lnum
      (start.pos_cnum - start.pos_bol)
      message

let () =
  let lexbuf = Sedlexing.Utf8.from_channel stdin in
  Sedlexing.set_filename lexbuf "<input>";

  try
    let program =
      lexbuf
      |> Sedlexing.with_tokenizer Lexer.token
      |> MenhirLib.Convert.Simplified.traditional2revised Parser.program
      |> Surface.elab_program
    in
    Format.printf "@[%a@]" Core.pp_print_program program;
  with
  | Lexer.UnexpectedChar -> print_error (Sedlexing.lexing_positions lexbuf) "unexpected character"; exit 1
  | Lexer.UnclosedBlockComment -> print_error (Sedlexing.lexing_positions lexbuf) "unclosed block comment"; exit 1
  | Parser.Error -> print_error (Sedlexing.lexing_positions lexbuf) "syntax error"; exit 1
