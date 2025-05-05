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

end

module Make (T : Set.S) : S
  with type token = T.elt
  with type token_set = T.t
= struct

  type token = T.elt
  type token_set = T.t

  (** Deterministic syntax descriptions *)
  type 'a syntax = {
    pure : 'a option;
    (** Value associated with the empty token stream *)

    alt : (token_set * 'a syntax_k) list;
    (** Syntaxes associated a token stream of one or more tokens *)
  }

  (** Syntax descriptions with a “hole” in them *)
  and 'a syntax_k =
    | Elem : token syntax_k
    | Seq1 : 'a * 'b syntax_k -> ('a * 'b) syntax_k
    | Seq2 : 'a syntax_k * 'b syntax -> ('a * 'b) syntax_k
    | Map : ('a -> 'b) * 'a syntax_k -> 'b syntax_k

  let syntax pure alt = { pure; alt }
  let elem = Elem
  let seq1 x1 sk = Seq1 (x1, sk)
  let seq2 sk s = Seq2 (sk, s)
  let map f s = Map (f, s)

  let rec parse : type a. a syntax -> token Seq.t -> (a * token Seq.t) option =
    let open Option.Notation in
    fun s ts ->
      match Seq.uncons ts with
      | None -> s.pure |> Option.map (fun x -> x, Seq.empty)
      | Some (t, ts) ->
          let* (_, sk) = s.alt |> List.find_opt (fun (tk, _) -> T.mem t tk) in
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

end
