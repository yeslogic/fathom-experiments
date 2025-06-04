open Simple_ddl

module Source_file = struct

  type t = {
    name : string;
    contents : string;
    lines : (int * int) Dynarray.t;
  }

  let create (name : string) (contents : string) : t =
    let lines = Dynarray.create () in
    let add_line stop =
      match Dynarray.find_last lines with
      | None -> Dynarray.add_last lines (0, stop)
      | Some (_, prev_stop) -> Dynarray.add_last lines (prev_stop + 1, stop)
    in
    contents |> String.iteri (fun pos ch -> if ch = '\n' then add_line pos);
    add_line (String.length contents);

    { name; contents; lines }

  let get_line (source : t) (line : int) : string =
    let start, stop = Dynarray.get source.lines (line - 1) in
    String.sub source.contents start (stop - start)

end

let format_diagnostic (source : Source_file.t) (severity : string) (start, stop : Surface.loc) (message : string) =
  let start_line, start_column = start.pos_lnum, start.pos_cnum - start.pos_bol in
  let stop_line, stop_column = stop.pos_lnum, stop.pos_cnum - stop.pos_bol in

  let gutter_num = Int.to_string start_line in
  let gutter_pad = String.map (Fun.const ' ') gutter_num in

  let underline_pad = String.make start_column ' ' in
  let underline =
    if start_line <> stop_line || stop_column <= start_column then "^" else
      String.make (stop_column - start_column) '^'
  in

  let buffer = Buffer.create 16 in

  Printf.bprintf buffer "%s: %s\n" severity message;
  Printf.bprintf buffer "%s ┌─ %s:%d:%d\n" gutter_pad source.name start_line start_column;
  Printf.bprintf buffer "%s │\n" gutter_pad;
  Printf.bprintf buffer "%s │ %s\n" gutter_num (Source_file.get_line source start_line);
  Printf.bprintf buffer "%s │ %s%s\n" gutter_pad underline_pad underline;

  Buffer.contents buffer

let elab_program (source : Source_file.t) =
  Printexc.record_backtrace true;

  let lexbuf = Sedlexing.Utf8.from_string source.contents in

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

      let source = Source_file.create "<input>" (get_source ()) in

      match elab_program source with
      | Ok program ->
          Format.asprintf "%a\n" Core.pp_program program
          |> set_output
      | Error (loc, message) ->
          format_diagnostic source "error" loc message
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

      let source = Source_file.create "<input>" (get_source ()) in

      match elab_program source with
      | Ok program ->
          Core.Compile.compile_program program
          |> Format.asprintf "%a\n" Rust.pp_program
          |> set_output
      | Error (loc, message) ->
          format_diagnostic source "error" loc message
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
