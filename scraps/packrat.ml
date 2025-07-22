(** Packrat parsing

    The idea behind Packrat parsing is to reduce the exponential cost of
    backtracking in top-down parsers. This is done by memoizing the results of
    parsing nonterminals at various locations in the source string, trading
    linear time parsing for memory consumption.

    - {{: https://doi.org/10.1145/581478.581483} Packrat parsing: simple, powerful, lazy, linear time, functional pearl}
    - {{: https://ohmjs.org/pubs/sle2017/incremental-packrat-parsing.pdf} Incremental Packrat Parsing}
    - {{: https://pdubroy.github.io/200andchange/packrat-parsing/} Packrat parsing (PEG style)}
    - {{: https://ohmjs.org/} Ohm}
*)

module Parse_tree = struct

  type 'id t =
    | Char of char
    | String of string
    | Rule of 'id * 'id t
    | Iter of 'id t list

  let rec pp tree ppf =
    match tree with
    | Char ch -> Format.fprintf ppf "'%c'" ch
    | String str -> Format.fprintf ppf "\"%s\"" str
    | Rule (_, tree) -> pp tree ppf (* FIXME: Print rules *)
    | Iter [] -> Format.fprintf ppf "[]"
    | Iter trees ->
        Format.fprintf ppf "@[<hv 2>[ %a ]@]"
          (Format.pp_print_list
            ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
            (fun ppf tree -> Format.fprintf ppf "%t" (pp tree)))
          trees

end

module Parser : sig

  type 'id t

  val create :
    rules:('id -> 'id t) ->
    root:'id ->
    string ->
    'id Parse_tree.t option

  val rule : 'id -> 'id t
  val terminal : string -> 'id t
  val choice : 'id t list -> 'id t
  val sequence : 'id t list -> 'id t
  val not : 'id t -> 'id t [@@warning "-unused-value-declaration"]
  val many : 'id t -> 'id t
  val many1 : 'id t -> 'id t
  val range : char -> char -> 'id t

end = struct

  type 'id memo_col = ('id, ('id Parse_tree.t * int) option) Hashtbl.t
  type 'id memo_table = (int, 'id memo_col) Hashtbl.t

  type 'id matcher = {
    rules : 'id -> 'id t;
    input : string;
    memo_table : 'id memo_table;
  }

  and 'id t =
    matcher:'id matcher -> pos:int -> ('id Parse_tree.t * int) option

  let create ~(rules : 'id -> 'id t) ~root:(id : 'id) (input : string) : 'id Parse_tree.t option =
    match rules id ~matcher:{ rules; input; memo_table = Hashtbl.create 16 } ~pos:0 with
    | Some (tree, pos) when String.length input = pos ->
        Some (Parse_tree.Rule (id, tree))
    | Some _ | None -> None

  let rule (id : 'id) : 'id t =
    fun ~matcher ~pos ->
      (* Check if there is a column in the memo table associated with the
         current position in the input string *)
      let result =
        match Hashtbl.find_opt matcher.memo_table pos with
        | Some col ->
            (* Check to see if the current rule has been previously
              parsed at this position in the input string *)
            begin match Hashtbl.find_opt col id with
            | Some result -> result
            | None ->
                (* Parse the rule at the current position and record the result in
                  the memoization table *)
                let result = matcher.rules id ~matcher ~pos in
                Hashtbl.add col id result;
                result
            end

        | None ->
            (* Parse the input from the current position using the requested
              rule, and crate a new column for it in the memo table *)
            let result = matcher.rules id ~matcher ~pos in
            let col = Hashtbl.create 1 in
            Hashtbl.add col id result;
            Hashtbl.add matcher.memo_table pos col;
            result
      in
      result |> Option.map @@ fun (tree, pos) ->
        (Parse_tree.Rule (id, tree), pos)

  let terminal (s : string) : 'id t =
    (* TODO: Check that length handling makes sense *)
    let len = String.length s in
    fun ~matcher ~pos ->
      let rec go n =
        if n < len then
          if matcher.input.[pos + n] = s.[n] then
            (go [@tailcall]) (n + 1)
          else None
        else
          Some (Parse_tree.String s, pos + n)
      in
      if pos + len <= String.length matcher.input then go 0 else None

  let rec choice (ps : 'id t list) : 'id t =
    fun ~matcher ~pos ->
      match ps with
      | p :: ps ->
          let result = p ~matcher ~pos in
          if Option.is_some result then result else
            (choice [@tailrec]) ps ~matcher ~pos
      | [] -> None

  let sequence (ps : 'id t list) : 'id t =
    let rec go ps acc =
      fun ~matcher ~pos ->
        match ps with
        | p :: ps ->
            begin match p ~matcher ~pos with
            | Some (tree, pos) -> (go [@tailcall]) ps (tree :: acc) ~matcher ~pos
            | None -> None
            end
        | [] ->
            Some (List.rev acc, pos)
    in
    fun ~matcher ~pos ->
      match go ps [] ~matcher ~pos with
      | Some (trees, pos) -> Some (Parse_tree.Iter trees, pos)
      | None -> None

  let not (p : 'id t) : 'id t =
    fun ~matcher ~pos ->
      match p ~matcher ~pos with
      | None -> Some (Parse_tree.Iter [], pos)
      | Some _ -> None

  let many (p : 'id t) : 'id t =
    let rec go acc =
      fun ~matcher ~pos ->
        match p ~matcher ~pos with
        | Some (tree, pos) -> (go [@tailcall]) (tree :: acc) ~matcher ~pos
        | None -> List.rev acc, pos
    in
    fun ~matcher ~pos ->
      let trees, pos = go [] ~matcher ~pos in
      Some (Parse_tree.Iter trees, pos)

  let many1 (p : 'id t) : 'id t =
    fun ~matcher ~pos ->
      match many p ~matcher ~pos with
      | Some (Parse_tree.Iter (_ :: _), _) as result -> result
      | Some _ | None -> None

  let range (low : char) (high : char) : 'id t =
    fun ~matcher ~pos ->
      if pos < String.length matcher.input then
        let ch = matcher.input.[pos] in
        if low <= ch && ch <= high then
          Some (Char ch, pos + 1)
        else None
      else
        None

end

let arithmetic1 =
  Parser.(create ~root:`Expr ~rules:begin
    function
    | `Expr ->
        choice [
          sequence [rule `PrimExpr; terminal "+"; rule `Expr];  (* plus *)
          sequence [rule `PrimExpr; terminal "-"; rule `Expr];  (* minus *)
          rule `PrimExpr;
        ]
    | `PrimExpr ->
        choice [
          sequence [terminal "("; rule `Expr; terminal ")"];    (* paren *)
          sequence [terminal "+"; rule `PrimExpr];              (* pos *)
          sequence [terminal "-"; rule `PrimExpr];              (* neg *)
          rule `Number;
        ]
    | `Number -> many1 (rule `Digit)
    | `Digit -> range '0' '9'
  end)

let () = begin
  let try_parse str =
    let tree = arithmetic1 str |> Option.get in
    Format.printf "@[<v 2>@[PARSE \"%s\":@]@,@,@[%t@]@]@.@." str (Parse_tree.pp tree)
  in

  Format.printf "Running arithmetic1 tests:@.@.";
  try_parse "7";
  try_parse "869";
  try_parse "869-72";
  try_parse "1+(869+33)";
  try_parse "(869+(-33)-4)-72";
  Format.printf "Done!@.@.";
end

let arithmetic2 =
  (* TODO: Handle whitespace - maybe use a separate lexer *)
  Parser.(create ~root:`Expr ~rules:begin
    function
    | `Expr -> rule `AddExpr
    | `AddExpr ->
        choice [
          sequence [rule `MulExpr; terminal "+"; rule `AddExpr];  (* plus *)
          sequence [rule `MulExpr; terminal "-"; rule `AddExpr];  (* minus *)
          rule `MulExpr;
        ]
    | `MulExpr ->
        choice [
          sequence [rule `ExpExpr; terminal "+"; rule `MulExpr];  (* times *)
          sequence [rule `ExpExpr; terminal "-"; rule `MulExpr];  (* divide *)
          rule `ExpExpr;
        ]
    | `ExpExpr ->
        choice [
          sequence [rule `PrimExpr; terminal "^"; rule `ExpExpr]; (* power *)
          rule `PrimExpr;
        ]
    | `PrimExpr ->
        choice [
          sequence [terminal "("; rule `Expr; terminal ")"];      (* paren *)
          sequence [terminal "+"; rule `PrimExpr];                (* pos *)
          sequence [terminal "-"; rule `PrimExpr];                (* neg *)
          rule `Ident;
          rule `Number;
        ]
    | `Ident ->
        sequence [
          rule `Letter;
          many (choice [rule `Letter; rule `Digit]);
        ];
    | `Number ->
        choice [
          sequence [many (rule `Digit); terminal "."; many1 (rule `Digit)]; (* fract *)
          many1 (rule `Digit);                                              (* whole *)
        ]
    | `Letter -> choice [range 'a' 'z'; range 'A' 'Z']
    | `Digit -> range '0' '9'
    (* | `Spaces -> many (choice [terminal " "; terminal "\t"; terminal "\n"]) *)
  end)
  [@@warning "-unused-value-declaration"]

let () = begin
  let try_parse str =
    let tree = arithmetic2 str |> Option.get in
    Format.printf "@[<v 2>@[PARSE \"%s\":@]@,@,@[%t@]@]@.@." str (Parse_tree.pp tree)
  in

  Format.printf "Running arithmetic2 tests:@.@.";
  try_parse "7";
  try_parse "869";
  try_parse "869-72";
  try_parse "1+(869+33)";
  try_parse "(869+(-33)-4)-72";
  Format.printf "Done!@.";
end
