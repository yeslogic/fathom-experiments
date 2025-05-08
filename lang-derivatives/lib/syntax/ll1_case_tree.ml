(** Top-down parsers with a single token of lookahead, expressed as case trees. *)

module type S = sig

  type token
  type token_set

  type _ t
  type _ cont

  val make : 'a option -> (token_set * 'a cont) list -> 'a t

  val token : token cont
  val seq1 : 'a -> 'b cont -> ('a * 'b) cont
  val seq2 : 'a cont -> 'b t -> ('a * 'b) cont
  val map : ('a -> 'b) -> 'a cont -> 'b cont

  val parse : 'a t -> token Seq.t -> 'a option

  (* TODO: Compile to Rust *)

  val emit_dot : 'a t -> (token_set -> string) -> unit

end

module Make (T : Set.S) : S
  with type token = T.elt
  with type token_set = T.t
= struct

  type token = T.elt
  type token_set = T.t

  type 'a t = {
    null : 'a option;
    cases : (token_set * 'a cont) list;
  }

  (** Syntax descriptions that can have a token applied to them *)
  and 'a cont =
    | Token : token cont
    | Seq1 : 'a * 'b cont -> ('a * 'b) cont
    | Seq2 : 'a cont * 'b t -> ('a * 'b) cont
    | Map : ('a -> 'b) * 'a cont -> 'b cont

  let make null cases = { null; cases }
  let token = Token
  let seq1 x1 sk = Seq1 (x1, sk)
  let seq2 sk s = Seq2 (sk, s)
  let map f s = Map (f, s)

  let rec parse : type a. a t -> token Seq.t -> (a * token Seq.t) option =
    let open Option.Notation in
    fun s ts ->
      match Seq.uncons ts with
      | None -> s.null |> Option.map (fun x -> x, Seq.empty)
      | Some (t, ts) ->
          let* (_, sk) = s.cases |> List.find_opt (fun (tk, _) -> T.mem t tk) in
          parse_cont sk t ts

  and parse_cont : type a. a cont -> token -> token Seq.t -> (a * token Seq.t) option =
    let open Option.Notation in
    fun sk t ts ->
      match sk with
      | Token -> Some (t, ts)
      | Seq1 (x1, sk) ->
          let+ (x2, ts) = parse_cont sk t ts in
          ((x1, x2), ts)
      | Seq2 (sk, s) ->
          let* (x1, ts) = parse_cont sk t ts in
          let+ (x2, ts) = parse s ts in
          ((x1, x2), ts)
      | Map (f, sk) ->
          let+ (x, ts) = parse_cont sk t ts in
          (f x, ts)

  let parse (type a) (s : a t) (ts : token Seq.t) : a option =
    let open Option.Notation in
    let* (x, ts) = parse s ts in
    if Seq.is_empty ts then Some x else None

  (* TODO: Test that this actually works! *)
  let emit_dot (type a) (s : a t) (token_set : token_set -> string) : unit =
    let fresh_node =
      let next_id = ref 0 in
      fun () ->
        let id = !next_id in
        incr next_id;
        id
    in

    let rec emit : type a. a t -> start:int -> stop:int -> unit =
      fun s ~start ~stop ->
        if Option.is_some s.null then
          Printf.printf "  %i -> %i [label = \"ε\"];\n" start stop;

        s.cases |> List.iter @@ fun (tk, sk) ->
          let next = fresh_node () in
          Printf.printf "  %i -> %i [label = \"%s\"];\n" start next (token_set tk);
          emit_cont sk ~start:next ~stop

    and emit_cont : type a. a cont -> start:int -> stop:int -> unit =
      fun sk ~start ~stop ->
        match sk with
        | Token -> ()
        | Seq1 (_, sk) ->
            emit_cont sk ~start ~stop
        | Seq2 (sk, s) ->
            let next = fresh_node () in
            emit_cont sk ~start ~stop:next;
            emit s ~start:next ~stop
        | Map (_, sk) ->
            emit_cont sk ~start ~stop

    in

    let start = fresh_node () in
    let stop = fresh_node () in

    Printf.printf "digraph ll1 {\n";
    Printf.printf "  rankdir=LR;\n";
    Printf.printf "  node [shape = doublecircle]; %i %i;\n" start stop;
    Printf.printf "  node [shape = circle];\n";
    emit s ~start ~stop;
    Printf.printf "}\n"

end
