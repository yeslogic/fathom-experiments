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

let emit (source : Source_file.t) (severity : string) (start, stop : Surface.loc) (message : string) =
  let start_line, start_column = start.pos_lnum, start.pos_cnum - start.pos_bol in
  let stop_line, stop_column = stop.pos_lnum, stop.pos_cnum - stop.pos_bol in

  let gutter_num = Int.to_string start_line in
  let gutter_pad = String.map (Fun.const ' ') gutter_num in

  let underline_pad = String.make start_column ' ' in
  let underline =
    if start_line <> stop_line || stop_column <= start_column then "^" else
      String.make (stop_column - start_column) '^'
  in

  Printf.eprintf "%s: %s\n" severity message;
  Printf.eprintf "%s ┌─ %s:%d:%d\n" gutter_pad source.name start_line start_column;
  Printf.eprintf "%s │\n" gutter_pad;
  Printf.eprintf "%s │ %s\n" gutter_num (Source_file.get_line source start_line);
  Printf.eprintf "%s │ %s%s\n" gutter_pad underline_pad underline

let () =
  let source = Source_file.create "<stdin>" (In_channel.input_all stdin) in
  let lexbuf = Sedlexing.Utf8.from_string source.contents in

  let program =
    try
      lexbuf
      |> Sedlexing.with_tokenizer Lexer.token
      |> MenhirLib.Convert.Simplified.traditional2revised Parser.program
      |> Surface.Elab.check_program
    with
    | Lexer.Unexpected_char -> emit source "error" (Sedlexing.lexing_positions lexbuf) "unexpected character"; exit 1
    | Lexer.Unclosed_block_comment -> emit source "error" (Sedlexing.lexing_positions lexbuf) "unclosed block comment"; exit 1
    | Parser.Error -> emit source "error" (Sedlexing.lexing_positions lexbuf) "syntax error"; exit 1
    | Surface.Elab.Error (loc, message) -> emit source "error" loc message; exit 1
    | Surface.Elab.Bug (loc, message) -> emit source "bug" loc message; exit 1
  in

  Format.printf "@[%a@]" Core.pp_print_program program
