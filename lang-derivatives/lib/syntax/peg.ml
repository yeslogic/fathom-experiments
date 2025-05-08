(** Top-down parsers with full backtracking and left-biased alternation. This is
    effectively a simplified form of {i parsing expression grammar} (PEG).
*)

module type S = sig

  type token
  type token_set

  type _ t

  val token : token_set -> token t
  val fail : 'a t
  val pure : 'a -> 'a t
  val alt : 'a t -> 'a t -> 'a t
  val seq : 'a t -> 'b t -> ('a * 'b) t
  val many0 : 'a t -> 'a list t
  val many1 : 'a t -> 'a list t
  val map : ('a -> 'b) -> 'a t -> 'b t

  val parse : 'a t -> token Seq.t -> 'a option

end

module Make (T : Set.S) : S
  with type token = T.elt
  with type token_set = T.t
= struct

  type token = T.elt
  type token_set = T.t

  type _ t =
    | Token : token_set -> token t
    | Fail : 'a t
    | Pure : 'a -> 'a t
    | Alt : 'a t * 'a t -> 'a t
    | Seq : 'a t * 'b t -> ('a * 'b) t
    | Many : 'a t -> 'a list t
    | Map : ('a -> 'b) * 'a t -> 'b t
    (* TODO: variables *)

  let token t = Token t
  let fail = Fail
  let pure x = Pure x
  let alt s1 s2 = Alt (s1, s2)
  let seq s1 s2 = Seq (s1, s2)
  let map f s = Map (f, s)

  let many0 s = Many s
  let many1 s = seq s (many0 s) |> map (fun (x, xs) -> x :: xs)

  let rec parse : type a. a t -> token Seq.t -> (a * token Seq.t) option =
    let open Option.Notation in
    fun s ts ->
      match s with
      | Token tk ->
          let* (t, ts) = Seq.uncons ts in
          if T.mem t tk then Some (t, ts) else None
      | Fail -> None
      | Pure x -> Some (x, ts)
      | Alt (s1, s2) ->
          Option.alt (parse s1 ts) (fun () -> parse s2 ts)
      | Seq (s1, s2) ->
          let* (x1, ts) = parse s1 ts in
          let+ (x2, ts) = parse s2 ts in
          ((x1, x2), ts)
      | Many s ->
          let rec go ts rev_xs =
            let* (x, ts) = parse s ts in
            go ts (x :: rev_xs)
          in
          let+ (xs, ts) = go ts [] in
          (List.rev xs, ts)
      | Map (f, s) ->
          let+ (x, ts) = parse s ts in
          (f x, ts)


  let parse (type a) (s : a t) (ts : token Seq.t) : a option =
    let open Option.Notation in
    let* (x, ts) = parse s ts in
    if Seq.is_empty ts then Some x else None

end

module Char = Make (Set.Char)
