let print_error (severity : string) (start, _ : Lexing.position * Lexing.position) (message : string) =
  Printf.eprintf "%s:%d:%d: %s: %s\n"
      start.pos_fname
      start.pos_lnum
      (start.pos_cnum - start.pos_bol)
      severity
      message

let () =
  let lexbuf = Sedlexing.Utf8.from_channel stdin in
  Sedlexing.set_filename lexbuf "<input>";

  try
    let program =
      lexbuf
      |> Sedlexing.with_tokenizer Lexer.token
      |> MenhirLib.Convert.Simplified.traditional2revised Parser.program
      |> Surface.Elab.check_program
    in
    Format.printf "@[%a@]" Core.pp_print_program program;
  with
  | Lexer.UnexpectedChar -> print_error "error" (Sedlexing.lexing_positions lexbuf) "unexpected character"; exit 1
  | Lexer.UnclosedBlockComment -> print_error "error" (Sedlexing.lexing_positions lexbuf) "unclosed block comment"; exit 1
  | Parser.Error -> print_error "error" (Sedlexing.lexing_positions lexbuf) "syntax error"; exit 1
  | Surface.Elab.Error (loc, message) -> print_error "error" loc message; exit 1
  | Surface.Elab.Bug (loc, message) -> print_error "bug" loc message; exit 1
