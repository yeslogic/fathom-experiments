open Simple_ddl

let print_error (severity : string) (start, _ : Lexing.position * Lexing.position) (message : string) =
  Printf.eprintf "%s:%d:%d: %s: %s\n"
      start.pos_fname
      start.pos_lnum
      (start.pos_cnum - start.pos_bol)
      severity
      message

let elab_program (filename : string) (input : in_channel) =
  Printexc.record_backtrace true;

  let lexbuf = Sedlexing.Utf8.from_channel input in
  Sedlexing.set_filename lexbuf filename;

  try
    lexbuf
    |> Sedlexing.with_tokenizer Lexer.token
    |> MenhirLib.Convert.Simplified.traditional2revised Parser.program
    |> Surface.Elab.check_program
    |> Core.zonk_program
    |> Result.ok
  with
  | Lexer.Unexpected_char -> Error (Sedlexing.lexing_positions lexbuf, "unexpected character")
  | Lexer.Unclosed_block_comment -> Error (Sedlexing.lexing_positions lexbuf, "unclosed block comment")
  | Parser.Error -> Error (Sedlexing.lexing_positions lexbuf, "syntax error")
  | Surface.Elab.Error (loc, message) -> Error (loc, message)

(** {1 Subcommands} *)

let elab_cmd () : unit =
  match elab_program "<input>" stdin with
  | Ok program ->
      program
      |> Format.printf "@[%a@]" Core.pp_program
  | Error (loc, message) ->
      print_error "error" loc message;
      exit 1

let compile_cmd () : unit =
  match elab_program "<input>" stdin with
  | Ok program ->
      program
      |> Core.Compile.compile_program
      |> Format.printf "@[%a@]" Rust.pp_program
  | Error (span, message) ->
      print_error "error" span message;
      exit 1

(** {1 CLI options} *)

let cmd =
  let open Cmdliner in

  Cmd.group (Cmd.info "simple-ddl") [
    Cmd.v (Cmd.info "elab" ~doc:"elaborate a program from standard input")
      Term.(const elab_cmd $ const ());
    Cmd.v (Cmd.info "compile" ~doc:"compile a program from standard input to rust")
      Term.(const compile_cmd $ const ());
  ]

(** {1 Main entrypoint} *)

let () =
  Printexc.record_backtrace true;
  exit (Cmdliner.Cmd.eval cmd)
