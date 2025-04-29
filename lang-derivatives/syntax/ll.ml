(** Backtracking, recursive descent parser semantics *)

module type S = sig

  type token
  type token_set

  type _ t

  val elem : token_set -> token t
  val fail : 'a t
  val pure : 'a -> 'a t
  val alt : 'a t -> 'a t -> 'a t
  val seq : 'a t -> 'b t -> ('a * 'b) t
  val map : ('a -> 'b) -> 'a t -> 'b t

  val parse : 'a t -> token Seq.t -> ('a * token Seq.t) option

end

module Make (T : Set.S) : S
  with type token = T.elt
  with type token_set = T.t
= struct

  type token = T.elt
  type token_set = T.t

  type _ t =
    | Elem : token_set -> token t
    | Fail : 'a t
    | Pure : 'a -> 'a t
    | Alt : 'a t * 'a t -> 'a t
    | Seq : 'a t * 'b t -> ('a * 'b) t
    | Map : ('a -> 'b) * 'a t -> 'b t
    (* TODO: variables *)

  let elem t = Elem t
  let fail = Fail
  let pure x = Pure x
  let alt s1 s2 = Alt (s1, s2)
  let seq s1 s2 = Seq (s1, s2)
  let map f s = Map (f, s)

  let rec parse : type a. a t -> token Seq.t -> (a * token Seq.t) option =
    let open Option.Notation in
    fun s ts ->
      match s with
      | Elem tk ->
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
      | Map (f, s) ->
          let+ (x, ts) = parse s ts in
          (f x, ts)

end

module Char = Make (Set.Char)
