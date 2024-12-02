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
  | Lexer.Unexpected_char -> Error (Sedlexing.lexing_positions lexbuf, "unexpected character")
  | Lexer.Unclosed_block_comment -> Error (Sedlexing.lexing_positions lexbuf, "unclosed block comment")
  | Parser.Error -> Error (Sedlexing.lexing_positions lexbuf, "syntax error")
  | Surface.Elab.Error (loc, message) -> Error (loc, message)


open Brr

module Component = struct
  (** Approach inspired by {{:https://github.com/abuseofnotation/vanilla-fp} vanilla-fp}. *)

  type 'st t = state:'st -> set_state:('st -> unit) -> El.t

  let rec render : type st. El.t -> st t -> st -> unit =
    fun elem component state ->
      El.set_children elem [
        component ~state ~set_state:(fun state ->
          print_endline "setting state";
          render elem component state);
      ]

end

let elab_button ~source ~set_output () =
  let on_click _ =
    print_endline "elaborate";

    let output =
      match elab_program "<input>" source with
      | Ok program -> Format.asprintf "%a\n" Core.pp_program program
      | Error (loc, message) -> format_error "error" loc message
    in

    set_output output;
  in

  let elem = El.button ~at:At.[id (Jstr.v "elab")] [El.txt' "Elaborate"] in
  ignore (El.as_target elem |> Ev.(listen click) on_click);
  elem

let compile_button ~source ~set_output () =
  let on_click _ =
    print_endline "compile";

    let output =
      match elab_program "<input>" source with
      | Ok program ->
          Core.Compile.compile_program program
          |> Format.asprintf "%a\n" Rust.pp_program
      | Error (loc, message) -> format_error "error" loc message
    in

    set_output output;
  in

  let elem = El.button ~at:At.[id (Jstr.v "compile")] [El.txt' "Compile"] in
  ignore (El.as_target elem |> Ev.(listen click) on_click);
  elem

let example_select ~set_source () =
  let on_input event =
    let name = Jv.get (Ev.target event |> Ev.target_to_jv) "value" |> Jv.to_string in
    print_endline ("select example: " ^ name);
    set_source (List.assoc name Examples.all);
  in

  let elem =
    El.select (Examples.all |> List.map (fun (n, _) ->
      El.option ~at:At.[if' (n = Examples.initial) selected] [El.txt' n]))
  in
  ignore (El.as_target elem |> Ev.(listen input) on_input);
  elem

let source_editor ~source ~set_source () =
  let on_input event =
    let text = Jv.get (Ev.target event |> Ev.target_to_jv) "value" |> Jv.to_string in
    print_endline ("update input");
    (* FIXME: Full re-render results in focus being lost from textarea *)
    set_source text;
  in

  let elem =
    El.textarea
      ~at:At.[
        id (Jstr.v "input");
        rows 20;
        cols 80;
        spellcheck (Jstr.v "false");
      ]
      [ El.txt' source ]
  in
  ignore (El.as_target elem |> Ev.(listen input) on_input);
  elem

type app_state = {
  source : string;
  output : string;
}

let app ~state ~set_state =
  let source = state.source in
  let set_output s = set_state { state with output = s } in
  let set_source s = set_state { state with source = s } in

  El.div [
    El.h1 [ El.txt' "Simple DDL" ];
    El.nav [
      elab_button ~source ~set_output ();
      compile_button ~source ~set_output ();
      example_select ~set_source ();
    ];
    source_editor ~source ~set_source ();
    El.pre [ El.txt' state.output ];
  ]

let () =
  let elem = Document.find_el_by_id G.document (Jstr.v "app") |> Option.get in
  Component.render elem app {
    source = List.assoc Examples.initial Examples.all;
    output = "";
  }
