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
  "format u16le :=";
  "  let b0 <- byte;";
  "  let b1 <- byte;";
  "  pure Int (b0 | (b1 << 8));";
  "";
  "format image {";
  "  width <- u16le;";
  "  height <- u16le;";
  "  data <- repeat-len (width * height) byte;";
  "}";
]

open Brr

let el_value (el : El.t) : string =
  El.(prop Prop.value) el |> Jstr.to_string

let elab_button_el ~(input_el : El.t) ~(output_el : El.t) : El.t =
  let open El in

  let button_el =
    button ~at:At.[id (Jstr.v "elab")] [txt' "Elaborate"];
  in

  let _ : Ev.listener =
    El.as_target button_el |> Ev.(listen click) @@ fun _ ->
      print_endline "elaborate";

      let program =
        el_value input_el
        |> elab_program "<input>"
        |> Format.asprintf "%a\n" Core.pp_program
      in

      El.set_children output_el El.[
        txt' program;
      ];
  in

  button_el

let compile_button_el ~(input_el : El.t) ~(output_el : El.t) : El.t =
  let open El in

  let button_el =
    button ~at:At.[id (Jstr.v "compile")] [txt' "Compile"]
  in

  let _ : Ev.listener =
    El.as_target button_el |> Ev.(listen click) @@ fun _ ->
      print_endline "compile";

      let program =
        el_value input_el
        |> elab_program "<input>"
        |> Core.Compile.compile_program
        |> Format.asprintf "%a\n" Rust.pp_program
      in

      El.set_children output_el El.[
        txt' program;
      ];
  in

  button_el

let input_el () : El.t =
  let open El in

  textarea
    ~at:At.[
      id (Jstr.v "input");
      rows 20;
      cols 80;
      spellcheck (Jstr.v "false");
    ]
    [ txt' test ]


let output_el () : El.t =
  let open El in

  pre ~at:At.[id (Jstr.v "output")] []


let main_el () =
  let open El in

  let input_el = input_el () in
  let output_el = output_el () in

  let elab_button_el = elab_button_el ~input_el ~output_el in
  let compile_button_el = compile_button_el ~input_el ~output_el in

  El.v (Jstr.v "main") [
    h1 [txt' "Simple DDL"];

    nav [
      elab_button_el;
      compile_button_el;
    ];

    input_el;
    output_el;
  ]

let () = begin


  El.set_children (Document.body G.document) [
    main_el ();
  ];

end
