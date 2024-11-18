open Simple_ddl

let print_error (severity : string) (start, _ : Lexing.position * Lexing.position) (message : string) =
  Printf.eprintf "%s:%d:%d: %s: %s\n"
    start.pos_fname
    start.pos_lnum
    (start.pos_cnum - start.pos_bol)
    severity
    message

let elab_program (filename : string) (input : string) =
  Printexc.record_backtrace true;

  let lexbuf = Sedlexing.Utf8.from_string input in
  Sedlexing.set_filename lexbuf filename;

  try
    lexbuf
    |> Sedlexing.with_tokenizer Lexer.token
    |> MenhirLib.Convert.Simplified.traditional2revised Parser.program
    |> Surface.Elab.check_program
  with
  | Lexer.Unexpected_char -> print_error "error" (Sedlexing.lexing_positions lexbuf) "unexpected character"; exit 1
  | Lexer.Unclosed_block_comment -> print_error "error" (Sedlexing.lexing_positions lexbuf) "unclosed block comment"; exit 1
  | Parser.Error -> print_error "error" (Sedlexing.lexing_positions lexbuf) "syntax error"; exit 1
  | Surface.Elab.Error (loc, message) -> print_error "error" loc message; exit 1

let test = String.concat "\n" [
  "format image {";
  "  width <- byte;";
  "  height <- byte;";
  "  data <- repeat-len (width * height) byte;";
  "}";
]

let () = begin

  let open Brr in

  El.set_children (Document.body G.document) El.[
    h1 [txt' "Simple DDL"];

    div [
      button ~at:At.[id (Jstr.v "elab")] [txt' "Elaborate"];
      button ~at:At.[id (Jstr.v "compile")] [txt' "Compile"];
    ];

    textarea
      ~at:At.[
        id (Jstr.v "input");
        rows 20;
        cols 80;
        spellcheck (Jstr.v "false");
      ] [
        txt' test;
      ];

    pre ~at:At.[id (Jstr.v "output")] [
      txt' "output";
    ];
  ];

  let elab_button = Document.find_el_by_id G.document (Jstr.v "elab") |> Option.get in
  let compile_button = Document.find_el_by_id G.document (Jstr.v "compile") |> Option.get in
  let input_el = Document.find_el_by_id G.document (Jstr.v "input") |> Option.get in
  let output_el = Document.find_el_by_id G.document (Jstr.v "output") |> Option.get in

  ignore
    (Ev.listen Ev.click
      (fun _ ->
        print_endline "elaborate";
        let program =
          elab_program "<input>" El.(prop Prop.value input_el |> Jstr.to_string)
          |> Format.asprintf "%a\n" Core.pp_program
        in
        El.set_children output_el El.[
          txt' program;
        ])
      (El.as_target elab_button));

  ignore
    (Ev.listen Ev.click
      (fun _ ->
        print_endline "compile";
        let program =
          elab_program "<input>" El.(prop Prop.value input_el |> Jstr.to_string)
          |> Core.Compile.compile_program
          |> Format.asprintf "%a\n" Rust.pp_program
        in
        El.set_children output_el El.[
          txt' program;
        ])
      (El.as_target compile_button));

end
