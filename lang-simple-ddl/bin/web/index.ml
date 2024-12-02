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

(* TODO: create a wrapper for the [El] module that allows for event handlers to
   be passed as attributes. *)
let with_listener event f elem =
  let _ : Ev.listener = El.as_target elem |> Ev.listen event f in
  elem

module Component = struct
  (** Approach inspired by {{:https://github.com/abuseofnotation/vanilla-fp} vanilla-fp}. *)

  type 'st t = state:'st -> set_state:('st -> unit) -> El.t

  let rec render : type st. El.t -> st t -> st -> unit =
    fun elem component state ->
      El.set_children elem [
        component ~state ~set_state:(fun state ->
          print_endline "rendering component";
          render elem component state);
      ]

end

module Elab_button = struct

  let create ~get_source ~set_output =
    let on_click _ =
      print_endline "elaborate";

      match elab_program "<input>" (get_source ()) with
      | Ok program ->
          Format.asprintf "%a\n" Core.pp_program program
          |> set_output
      | Error (loc, message) ->
          format_error "error" loc message
          |> set_output
    in

    El.button ~at:At.[id (Jstr.v "elab")] [El.txt' "Elaborate"]
    |> with_listener Ev.click on_click

end

module Compile_button = struct

  let create ~get_source ~set_output =
    let on_click _ =
      print_endline "compile";

      match elab_program "<input>" (get_source ()) with
      | Ok program ->
          Core.Compile.compile_program program
          |> Format.asprintf "%a\n" Rust.pp_program
          |> set_output
      | Error (loc, message) ->
          format_error "error" loc message
          |> set_output
    in

    El.button ~at:At.[id (Jstr.v "compile")] [El.txt' "Compile"]
    |> with_listener Ev.click on_click

end

module Example_select = struct

  let create ~set_source =
    let on_input event =
      let name = Jv.get (Ev.target event |> Ev.target_to_jv) "value" |> Jv.to_string in
      print_endline ("select example: " ^ name);
      set_source (List.assoc name Examples.all);
    in

    El.select (Examples.all |> List.map (fun (n, _) ->
      El.option ~at:At.[if' (n = Examples.initial) selected] [El.txt' n]))
    |> with_listener Ev.input on_input

end

module Source_editor = struct

  let create ~get_source ~set_source =
    let on_input event =
      let text = Jv.get (Ev.target event |> Ev.target_to_jv) "value" |> Jv.to_string in
      print_endline ("update input");
      set_source text;
    in

    El.textarea
      ~at:At.[
        id (Jstr.v "input");
        rows 20;
        cols 80;
        spellcheck (Jstr.v "false");
        autofocus;
      ]
      [ El.txt' (get_source ()) ]
    |> with_listener Ev.input on_input

end

module App = struct

  type state = {
    mutable source : string;
    output : string;
  }

  let create : state Component.t =
    fun ~state ~set_state ->
      let get_source () = state.source in
      let set_output s = set_state { state with output = s } in
      let set_source s = set_state { state with source = s } in

      El.div [
        El.h1 [ El.txt' "Simple DDL" ];
        El.nav [
          Elab_button.create ~get_source ~set_output;
          Compile_button.create ~get_source ~set_output;
          Example_select.create ~set_source;
        ];
        Source_editor.create ~get_source
          (* NOTE: Avoid focus loss by mutating the state in place when editing
             the text area, which avoids triggering a re-render. This feels a
             bit hacky, but it works for now!

             As a result we need to be careful to pass the current state of the
             source code with [get_source], as opposed to naively copying it
             from the current state, which would get out of date. *)
          ~set_source:(fun s -> state.source <- s);
        El.pre [ El.txt' state.output ];
      ]

end

let () =
  let elem = Document.find_el_by_id G.document (Jstr.v "app") |> Option.get in
  Component.render elem App.create {
    source = List.assoc Examples.initial Examples.all;
    output = "";
  }
