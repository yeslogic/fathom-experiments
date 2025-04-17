module type S = sig

  include Core.S

  val parse : 'a t -> (token Seq.t -> 'a option) option

end

module Make (T : Token_set.S) : S
  with type token = T.elt
  with type token_set = T.t
= struct

  include Core.Make (T)

  let rec nullable : type a. a t -> a option =
    function
    | Elem _ -> None
    | Fail -> None
    | Pure x -> Some x
    | Alt (s1, s2) -> Option.alt (nullable s1) (fun () -> nullable s2)
    | Seq (s1, s2) -> Option.both (nullable s1) (nullable s2)
    | Map (f, s) -> nullable s |> Option.map f

  let rec is_nullable : type a. a t -> bool =
    (* fun s -> Option.is_some (nullable s) *)
    function
    | Elem _ -> false
    | Fail -> false
    | Pure _ -> true
    | Alt (s1, s2) -> is_nullable s1 || is_nullable s2
    | Seq (s1, s2) -> is_nullable s1 && is_nullable s2
    | Map (_, s) -> is_nullable s

  let rec is_productive : type a. a t -> bool =
    function
    | Elem _ -> true
    | Fail -> false
    | Pure _ -> true
    | Alt (s1, s2) -> is_productive s1 || is_productive s2
    | Seq (s1, s2) -> is_productive s1 && is_productive s2
    | Map (_, s) -> is_productive s

  let rec first : type a. a t -> token_set =
    function
    | Elem t -> t
    | Fail -> T.empty
    | Pure _ -> T.empty
    | Alt (s1, s2) ->
        T.union (first s1) (first s2)
    | Seq (s1, s2) ->
        T.empty
        |> T.union (if is_nullable s1 then first s2 else T.empty)
        |> T.union (if is_productive s2 then first s1 else T.empty)
    | Map (_, s) -> first s

  let rec should_not_follow : type a. a t -> token_set =
    function
    | Elem _ -> T.empty
    | Fail -> T.empty
    | Pure _ -> T.empty
    | Alt (s1, s2) ->
        T.union (should_not_follow s1) (should_not_follow s2)
        |> T.union (if is_nullable s2 then first s1 else T.empty)
        |> T.union (if is_nullable s1 then first s2 else T.empty)
    | Seq (s1, s2) ->
        T.empty
        |> T.union (if is_nullable s2 then should_not_follow s1 else T.empty)
        |> T.union (if is_productive s1 then should_not_follow s2 else T.empty)
    | Map (_, s) ->
        should_not_follow s

  let rec has_conflict : type a. a t -> bool =
    function
    | Elem _ -> false
    | Fail -> false
    | Pure _ -> false
    | Alt (s1, s2) ->
        has_conflict s1
          || has_conflict s2
          || (is_nullable s1 && is_nullable s2)
          || not (T.disjoint (first s1) (first s2))
    | Seq (s1, s2) ->
        has_conflict s1
          || has_conflict s2
          || not (T.disjoint (should_not_follow s1) (first s2))
    | Map (_, s) ->
        has_conflict s

  (** Returns the state of the syntax after seeing a token *)
  let rec derive : type a. token -> a t -> a t option =
    let open Option.Notation in
    fun t s ->
      match s with
      | Elem tk when T.mem t tk ->
          Some (Pure t)
      | Elem _ -> None
      | Fail -> None
      | Pure _ -> None
      | Alt (s1, s2) ->
          begin match T.mem t (first s1) with
          | true -> derive t s1
          | false -> derive t s2
          end
      | Seq (s1, s2) ->
          begin match nullable s1 with
          | Some x when T.mem t (first s2)->
              let* s2' = derive t s2 in
              Some (Seq (Pure x, s2'))
          | Some _ | None ->
              let* s1' = derive t s1 in
              Some (Seq (s1', s2))
          end
      | Map (f, s) ->
          let+ s' = derive t s in
          Map (f, s')

  let parse (type a) (s : a t) : (token Seq.t -> a option) option =
    let open Option.Notation in
    let rec parse : type a. a t -> token Seq.t -> a option =
      fun s ts ->
        match Seq.uncons ts with
        | None -> nullable s
        | Some (t, ts) ->
            if T.mem t (first s) then
              let* s' = derive t s in
              (parse [@tailcall]) s' ts
            else
              None
    in
    if has_conflict s then None else
      Some (parse s)

end

module Char = Make (Token_set.Char)
