(** An LL(1) parser language implemented with derivatives.

    {2 Resources}

    - Romain Edelmann, Jad Hamza, Viktor Kunčak, “Zippy LL(1) parsing with
      derivatives”, PLDI 2020, https://doi.org/10.1145/3385412.3385992
    - {{: https://github.com/epfl-lara/scallion} epfl-lara/scallion} on Github
    - {{: https://github.com/epfl-lara/scallion-proofs>} epfl-lara/scallion-proofs} on Github
*)

module type S = sig

  type token
  type token_set

  type _ syntax

  val elem : token_set -> token syntax
  val fail : 'a syntax
  val pure : 'a -> 'a syntax
  val alt : 'a syntax -> 'a syntax -> 'a syntax
  val seq : 'a syntax -> 'b syntax -> ('a * 'b) syntax
  val map : ('a -> 'b) -> 'a syntax -> 'b syntax

  val parse : 'a syntax -> (token Seq.t -> 'a option) option
  (** Returns a parse function for the provided syntax, provided it is free from
      LL(1) conflicts. *)

end

module Make (T : Set.S) : S
  with type token = T.elt
  with type token_set = T.t
= struct

  type token = T.elt
  type token_set = T.t

  type _ syntax =
    | Elem : token_set -> token syntax
    | Fail : 'a syntax
    | Pure : 'a -> 'a syntax
    | Alt : 'a syntax * 'a syntax -> 'a syntax
    | Seq : 'a syntax * 'b syntax -> ('a * 'b) syntax
    | Map : ('a -> 'b) * 'a syntax -> 'b syntax
    (* TODO: variables *)

  let elem t = Elem t
  let fail = Fail
  let pure x = Pure x
  let alt s1 s2 = Alt (s1, s2)
  let seq s1 s2 = Seq (s1, s2)
  let map f s = Map (f, s)

  (** Returns a value if the syntax associates an empty sequence of tokens with
      that value. In the case of multiple possibilities, the first is chosen,
      leaving the detection of ambiguities up to the {!has_conflict} predicate. *)
  let rec nullable : type a. a syntax -> a option =
    function
    | Elem _ -> None
    | Fail -> None
    | Pure x -> Some x
    | Alt (s1, s2) -> Option.alt (nullable s1) (fun () -> nullable s2)
    | Seq (s1, s2) -> Option.both (nullable s1) (nullable s2)
    | Map (f, s) -> nullable s |> Option.map f

  (** Returns [true] if the syntax might parse an empty sequence of tokens. *)
  let rec is_nullable : type a. a syntax -> bool =
    (* fun s -> Option.is_some (nullable s) *)
    function
    | Elem _ -> false
    | Fail -> false
    | Pure _ -> true
    | Alt (s1, s2) -> is_nullable s1 || is_nullable s2
    | Seq (s1, s2) -> is_nullable s1 && is_nullable s2
    | Map (_, s) -> is_nullable s

  (** Returns [true] if the syntax associates at least one sequence of tokens
      with a value. *)
  let rec is_productive : type a. a syntax -> bool =
    function
    | Elem _ -> true
    | Fail -> false
    | Pure _ -> true
    | Alt (s1, s2) -> is_productive s1 || is_productive s2
    | Seq (s1, s2) -> is_productive s1 && is_productive s2
    | Map (_, s) -> is_productive s

  let rec first : type a. a syntax -> token_set =
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

  let rec should_not_follow : type a. a syntax -> token_set =
    function
    | Elem _ -> T.empty
    | Fail -> T.empty
    | Pure _ -> T.empty
    | Alt (s1, s2) ->
        T.union (should_not_follow s1) (should_not_follow s2)
        (* Elements of the should-not-follow set are introduced below *)
        |> T.union (if is_nullable s2 then first s1 else T.empty)
        |> T.union (if is_nullable s1 then first s2 else T.empty)
    | Seq (s1, s2) ->
        T.empty
        (* If the trailing syntax is nullable, take the should-not-follow set
           from the preceding syntax *)
        |> T.union (if is_nullable s2 then should_not_follow s1 else T.empty)
        (* If the preceding syntax has a chance of succeeding, then take the
           should-not-follow set of the trailing syntax *)
        |> T.union (if is_productive s1 then should_not_follow s2 else T.empty)
    | Map (_, s) ->
        should_not_follow s

  (** Returns [true] if an LL1 conflict was found in the syntax *)
  let rec has_conflict : type a. a syntax -> bool =
    function
    | Elem _ -> false
    | Fail -> false
    | Pure _ -> false
    | Alt (s1, s2) ->
        (has_conflict s1 || has_conflict s2)
          (* There should only be one nullable branch *)
          || (is_nullable s1 && is_nullable s2)
          (* The branches of the alternation must be disjoint *)
          || not (T.disjoint (first s1) (first s2))
    | Seq (s1, s2) ->
        (has_conflict s1 || has_conflict s2)
          (* The first set of the trailing syntax should not contain any element
             from the should-not-follow set of the preceding syntax. s *)
          || not (T.disjoint (should_not_follow s1) (first s2))
    | Map (_, s) ->
        has_conflict s

  (** Returns the state of the syntax after seeing a token. This operation is
      {i not} tail-recursive, and the resulting derivative can grow larger than
      the original syntax. *)
  let rec derive : type a. token -> a syntax -> a syntax option =
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
          | Some x when T.mem t (first s2) ->
              let* s2' = derive t s2 in
              Some (Seq (Pure x, s2'))
          | Some _ | None ->
              let* s1' = derive t s1 in
              Some (Seq (s1', s2))
          end
      | Map (f, s) ->
          let+ s' = derive t s in
          Map (f, s')

  let parse (type a) (s : a syntax) : (token Seq.t -> a option) option =
    let open Option.Notation in
    let rec parse : type a. a syntax -> token Seq.t -> a option =
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

module Char = Make (Set.Char)
