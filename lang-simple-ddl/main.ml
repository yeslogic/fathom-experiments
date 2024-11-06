let print_error (severity : string) (start, _ : Lexing.position * Lexing.position) (message : string) =
  Printf.eprintf "%s:%d:%d: %s: %s\n"
      start.pos_fname
      start.pos_lnum
      (start.pos_cnum - start.pos_bol)
      severity
      message

let () =
  Printexc.record_backtrace true;

  let lexbuf = Sedlexing.Utf8.from_channel stdin in
  Sedlexing.set_filename lexbuf "<input>";

  try
    let rust_program =
      lexbuf
      |> Sedlexing.with_tokenizer Lexer.token
      |> MenhirLib.Convert.Simplified.traditional2revised Parser.program
      |> Surface.Elab.check_program
      |> Core.Compile.compile_program
    in
    Format.printf "@[%a@]"
      Rust.pp_program rust_program;
  with
  | Lexer.Unexpected_char -> print_error "error" (Sedlexing.lexing_positions lexbuf) "unexpected character"; exit 1
  | Lexer.Unclosed_block_comment -> print_error "error" (Sedlexing.lexing_positions lexbuf) "unclosed block comment"; exit 1
  | Parser.Error -> print_error "error" (Sedlexing.lexing_positions lexbuf) "syntax error"; exit 1
  | Surface.Elab.Error (loc, message) -> print_error "error" loc message; exit 1
