(** Top-down parsers with a single token of lookahead. *)

module type S = sig

  type token
  type token_set

  type _ syntax
  type _ syntax_k

  val syntax : 'a option -> (token_set * 'a syntax_k) list -> 'a syntax

  val elem : token syntax_k
  val seq1 : 'a -> 'b syntax_k -> ('a * 'b) syntax_k
  val seq2 : 'a syntax_k -> 'b syntax -> ('a * 'b) syntax_k
  val map : ('a -> 'b) -> 'a syntax_k -> 'b syntax_k

  val parse : 'a syntax -> token Seq.t -> 'a option

  (* TODO: Compile to Rust *)

  val emit_dot : 'a syntax -> (token_set -> string) -> unit

end

module Make (T : Set.S) : S
  with type token = T.elt
  with type token_set = T.t
= struct

  type token = T.elt
  type token_set = T.t

  (** LL(1) syntax descriptions *)
  type 'a syntax = {
    null : 'a option;
    cases : (token_set * 'a syntax_k) list;
  }

  (** Syntax descriptions that can have a token applied to them *)
  and 'a syntax_k =
    | Elem : token syntax_k
    | Seq1 : 'a * 'b syntax_k -> ('a * 'b) syntax_k
    | Seq2 : 'a syntax_k * 'b syntax -> ('a * 'b) syntax_k
    | Map : ('a -> 'b) * 'a syntax_k -> 'b syntax_k

  let syntax null cases = { null; cases }
  let elem = Elem
  let seq1 x1 sk = Seq1 (x1, sk)
  let seq2 sk s = Seq2 (sk, s)
  let map f s = Map (f, s)

  let rec parse : type a. a syntax -> token Seq.t -> (a * token Seq.t) option =
    let open Option.Notation in
    fun s ts ->
      match Seq.uncons ts with
      | None -> s.null |> Option.map (fun x -> x, Seq.empty)
      | Some (t, ts) ->
          let* (_, sk) = s.cases |> List.find_opt (fun (tk, _) -> T.mem t tk) in
          parse_k sk t ts

  and parse_k : type a. a syntax_k -> token -> token Seq.t -> (a * token Seq.t) option =
    let open Option.Notation in
    fun sk t ts ->
      match sk with
      | Elem -> Some (t, ts)
      | Seq1 (x1, sk) ->
          let+ (x2, ts) = parse_k sk t ts in
          ((x1, x2), ts)
      | Seq2 (sk, s) ->
          let* (x1, ts) = parse_k sk t ts in
          let+ (x2, ts) = parse s ts in
          ((x1, x2), ts)
      | Map (f, sk) ->
          let+ (x, ts) = parse_k sk t ts in
          (f x, ts)

  let parse (type a) (s : a syntax) (ts : token Seq.t) : a option =
    let open Option.Notation in
    let* (x, ts) = parse s ts in
    if Seq.is_empty ts then Some x else None

  (* TODO: Test that this actually works! *)
  let emit_dot (type a) (s : a syntax) (token_set : token_set -> string) : unit =
    let fresh_node =
      let next_id = ref 0 in
      fun () ->
        let id = !next_id in
        incr next_id;
        id
    in

    let rec emit_syntax : type a. a syntax -> start:int -> stop:int -> unit =
      fun s ~start ~stop ->
        if Option.is_some s.null then
          Printf.printf "  %i -> %i [label = \"ε\"];\n" start stop;

        s.cases |> List.iter @@ fun (tk, sk) ->
          let next = fresh_node () in
          Printf.printf "  %i -> %i [label = \"%s\"];\n" start next (token_set tk);
          emit_syntax_k sk ~start:next ~stop

    and emit_syntax_k : type a. a syntax_k -> start:int -> stop:int -> unit =
      fun sk ~start ~stop ->
        match sk with
        | Elem -> ()
        | Seq1 (_, sk) ->
            emit_syntax_k sk ~start ~stop
        | Seq2 (sk, s) ->
            let next = fresh_node () in
            emit_syntax_k sk ~start ~stop:next;
            emit_syntax s ~start:next ~stop
        | Map (_, sk) ->
            emit_syntax_k sk ~start ~stop

    in

    let start = fresh_node () in
    let stop = fresh_node () in

    Printf.printf "digraph ll1 {\n";
    Printf.printf "  rankdir=LR;\n";
    Printf.printf "  node [shape = doublecircle]; %i %i;\n" start stop;
    Printf.printf "  node [shape = circle];\n";
    emit_syntax s ~start ~stop;
    Printf.printf "}\n"

end
