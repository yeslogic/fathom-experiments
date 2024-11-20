open Simple_ddl

let format_error (severity : string) (start, _ : Lexing.position * Lexing.position) (message : string) =
  Printf.sprintf "%s:%d:%d: %s: %s\n"
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
    |> Result.ok
  with
  | Lexer.Unexpected_char -> Error (format_error "error" (Sedlexing.lexing_positions lexbuf) "unexpected character")
  | Lexer.Unclosed_block_comment -> Error (format_error "error" (Sedlexing.lexing_positions lexbuf) "unclosed block comment")
  | Parser.Error -> Error (format_error "error" (Sedlexing.lexing_positions lexbuf) "syntax error")
  | Surface.Elab.Error (loc, message) -> Error (format_error "error" loc message)

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

      let output_text =
        match el_value input_el |> elab_program "<input>" with
        | Ok program -> Format.asprintf "%a\n" Core.pp_program program
        | Error error -> error
      in

      El.set_children output_el El.[
        txt' output_text;
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

      let output_text =
        match el_value input_el |> elab_program "<input>" with
        | Ok program ->
            Core.Compile.compile_program program
            |> Format.asprintf "%a\n" Rust.pp_program
        | Error error -> error
      in

      El.set_children output_el El.[
        txt' output_text;
      ];
  in

  button_el


let example_select_el ~(input_el : El.t) : El.t =
  let open El in

  let select_el =
    select (Examples.all |> List.map (fun (n, _) ->
      option ~at:At.[if' (n = Examples.initial) selected] [txt' n]))
  in

  let _ : Ev.listener =
    El.as_target select_el |> Ev.(listen change) @@ fun _ ->
      let example_name = el_value select_el in

      print_endline ("select example: " ^ example_name);

      El.set_children input_el El.[
        txt' (List.assoc example_name Examples.all);
      ];
  in

  select_el


let input_el () : El.t =
  let open El in

  textarea
    ~at:At.[
      id (Jstr.v "input");
      rows 20;
      cols 80;
      spellcheck (Jstr.v "false");
    ]
    [ txt' (List.assoc Examples.initial Examples.all) ]


let output_el () : El.t =
  let open El in

  pre ~at:At.[id (Jstr.v "output")] []


let main_el () =
  let open El in

  let input_el = input_el () in
  let output_el = output_el () in

  El.v (Jstr.v "main") [
    h1 [txt' "Simple DDL"];

    nav [
      elab_button_el ~input_el ~output_el;
      compile_button_el ~input_el ~output_el;
      example_select_el ~input_el;
    ];

    input_el;
    output_el;
  ]


let () = begin

  El.set_children (Document.body G.document) [
    main_el ();
  ];

end
