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
          print_endline "rendering component";
          render elem component state);
      ]

end

module Elab_button = struct

  let create ~get_source ~set_output =
    let handle_click _ =
      print_endline "elaborate";

      match elab_program "<input>" (get_source ()) with
      | Ok program ->
          Format.asprintf "%a\n" Core.pp_program program
          |> set_output
      | Error (loc, message) ->
          format_error "error" loc message
          |> set_output
    in

    let open Html in
    let open Html.Attr in

    button [
      id "elab";
      on_click handle_click;
    ] [
      text "Elaborate";
    ]

end

module Compile_button = struct

  let create ~get_source ~set_output =
    let handle_click _ =
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

    (* failwith "help"; *)
    (* print_endline "hello!!"; *)

    let open Html in
    let open Html.Attr in

    button [
      id "compile";
      on_click handle_click;
    ] [
      text "Compile";
    ]

end

module Example_select = struct

  let create ~selected_example ~set_selected_example =
    let handle_input event =
      let name = Jv.get (Ev.target event |> Ev.target_to_jv) "value" |> Jv.to_string in
      print_endline ("select example: " ^ name);
      set_selected_example name;
    in

    let open Html in
    let open Html.Attr in

    select [
      on_input handle_input
    ] (Examples.all |> List.map (fun (n, _) ->
      option [set_if (n = selected_example) selected] [text n]))

end

module Source_input = struct

  let create ~get_source ~set_source =
    let handle_input event =
      let text = Jv.get (Ev.target event |> Ev.target_to_jv) "value" |> Jv.to_string in
      print_endline ("update input");
      set_source text;
    in

    let open Html in
    let open Html.Attr in

    textarea [
      wrap "off";
      spellcheck "false";
      on_input handle_input;
    ] [
      text (get_source ())
    ]

end

module App = struct

  type state = {
    mutable source : string;
    selected_example : string;
    output : string;
  }

  let create : state Component.t =
    fun ~state ~set_state ->
      let get_source () = state.source in
      let set_output s = set_state { state with output = s } in
      let selected_example = state.selected_example in
      let set_selected_example name =
        set_state { state with
          source = List.assoc name Examples.all;
          selected_example = name;
        }
      in

      let open Html in
      let open Html.Attr in

      div [ id "main" ] [
        nav [ id "toolbar" ] [
          Example_select.create ~selected_example ~set_selected_example;
          Elab_button.create ~get_source ~set_output;
          Compile_button.create ~get_source ~set_output;
        ];
        div [ id "editor" ] [
          Source_input.create ~get_source
            ~set_source:(fun s ->
              (* NOTE: Mutating the state in-place avoids triggering a re-render
                of the textarea, which would cause it to lose focus during text
                editing. This feels a bit hacky, but it works for now!

                Because we're using a mutable field we need to be careful to pass
                the current state of the source code with [get_source], as opposed
                to naively copying it from the current state, which might be out
                of date by the time we want to make use of it. *)
              state.source <- s);
        ];
        div [ id "output" ] [
          pre [] [ text state.output ];
        ];
    ]

end

let () =
  let elem = Document.find_el_by_id G.document (Jstr.v "app") |> Option.get in
  Component.render elem App.create {
    source = List.assoc Examples.initial Examples.all;
    selected_example = Examples.initial;
    output = "";
  }
