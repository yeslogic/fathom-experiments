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

let elab_program (source : Source_file.t) : (Core.program, Surface.loc * string) result =
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

let () =
  let module Js = Js_of_ocaml.Js in

  Js.export "lang" object%js

    val examples = object%js

      val initial =
        Js.string Examples.initial

      val all =
        Examples.all
        |> Array.map (fun (name, source) -> object%js
          val name = Js.string name
          val source = Js.string source
        end)
        |> Js.array

    end

    val driver = object%js

      method elabProgram (source : Js.(js_string t)) : Js.(js_string t) =
        let source = Source_file.create "<input>" (Js.to_string source) in

        match elab_program source with
        | Ok program ->
            Format.asprintf "%a\n" Core.pp_program program
            |> Js.string
        | Error (loc, message) ->
            format_diagnostic source "error" loc message
            |> Js.string

      method compileProgram (source : Js.(js_string t)) : Js.(js_string t) =
        let source = Source_file.create "<input>" (Js.to_string source) in

        match elab_program source with
        | Ok program ->
            Core.Compile.compile_program program
            |> Format.asprintf "%a\n" Rust.pp_program
            |> Js.string
        | Error (loc, message) ->
            format_diagnostic source "error" loc message
            |> Js.string

    end

  end
