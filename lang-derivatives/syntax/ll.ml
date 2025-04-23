module type S = sig

  include Core.S

  val parse : 'a t -> token Seq.t -> ('a * token Seq.t) option

end

module Make (T : Set.S) : S
  with type token = T.elt
  with type token_set = T.t
= struct

  include Core.Make (T)

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
