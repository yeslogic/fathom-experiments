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

  (** Top-down parser semantics, implemented with recursive-descent and backtracking *)
  module Rd : sig

    val parse : 'a t -> token Seq.t -> ('a * token Seq.t) option

  end

  (** LL(1) parser semantics, implemented with derivatives *)
  module Ll1 : sig

    val parse : 'a t -> (token Seq.t -> 'a option) option

  end

  (* TODO: Parsing with derivatives *)
  (* TODO: Zippy LL(1) parsing *)
  (* TODO: Compile NFA-style syntax descriptions to DFA-style syntax (like Daedalus) *)

end

module Make (T : Token.S) : S

  with type token = T.t
  with type token_set = T.Set.t

= struct

  type token = T.t
  type token_set = T.Set.t

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

  module Rd = struct

    let rec parse : type a. a t -> token Seq.t -> (a * token Seq.t) option =
      let open Option.Notation in
      fun s ts ->
        match s with
        | Elem tk ->
            let* (t, ts) = Seq.uncons ts in
            if T.Set.mem t tk then Some (t, ts) else None
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

  module Ll1 = struct

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
      | Fail -> T.Set.empty
      | Pure _ -> T.Set.empty
      | Alt (s1, s2) ->
          T.Set.union (first s1) (first s2)
      | Seq (s1, s2) ->
          T.Set.empty
          |> T.Set.union (if is_nullable s1 then first s2 else T.Set.empty)
          |> T.Set.union (if is_productive s2 then first s1 else T.Set.empty)
      | Map (_, s) -> first s

    let rec should_not_follow : type a. a t -> token_set =
      function
      | Elem _ -> T.Set.empty
      | Fail -> T.Set.empty
      | Pure _ -> T.Set.empty
      | Alt (s1, s2) ->
          T.Set.union (should_not_follow s1) (should_not_follow s2)
          |> T.Set.union (if is_nullable s2 then first s1 else T.Set.empty)
          |> T.Set.union (if is_nullable s1 then first s2 else T.Set.empty)
      | Seq (s1, s2) ->
          T.Set.empty
          |> T.Set.union (if is_nullable s2 then should_not_follow s1 else T.Set.empty)
          |> T.Set.union (if is_productive s1 then should_not_follow s2 else T.Set.empty)
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
            || not (T.Set.disjoint (first s1) (first s2))
      | Seq (s1, s2) ->
          has_conflict s1
            || has_conflict s2
            || not (T.Set.disjoint (should_not_follow s1) (first s2))
      | Map (_, s) ->
          has_conflict s

    (** Returns the state of the syntax after seeing a token *)
    let rec derive : type a. token -> a t -> a t option =
      let open Option.Notation in
      fun t s ->
        match s with
        | Elem tk when T.Set.mem t tk ->
            Some (Pure t)
        | Elem _ -> None
        | Fail -> None
        | Pure _ -> None
        | Alt (s1, s2) ->
            begin match T.Set.mem t (first s1) with
            | true -> derive t s1
            | false -> derive t s2
            end
        | Seq (s1, s2) ->
            begin match nullable s1 with
            | Some x when T.Set.mem t (first s2)->
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
      let rec parse : type a. a t -> token Seq.t -> a option =
        let open Option.Notation in
        fun s ts ->
          match Seq.uncons ts with
          | None -> nullable s
          | Some (t, ts) ->
              if T.Set.mem t (first s) then
                let* s' = derive t s in
                (parse [@tailcall]) s' ts
              else
                None
      in
      if has_conflict s then None else
        Some (parse s)

  end

end

module Char = Make (Token.Char)
