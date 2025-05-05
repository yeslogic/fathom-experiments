(** Syntax descriptions that are guaranteed to be LL(1) when they are
    constructed. They can either be parsed with derivatives or compiled to
    {!Ll1} syntax descriptions for improved performance.

    The approach to checking parsing with derivatives and checking for LL(1)
    conflicts was inspired by {{: https://doi.org/10.1145/3385412.3385992}
    “Zippy LL(1) parsing with derivatives”}.
*)

module type S = sig

  type token
  type token_set

  type _ syntax

  exception Nullable_conflict
  exception First_conflict
  exception Follow_conflict

  val elem : token_set -> token syntax
  val fail : 'a syntax
  val pure : 'a -> 'a syntax
  val alt : 'a syntax -> 'a syntax -> 'a syntax (* Nullable_conflict, First_conflict *)
  val seq : 'a syntax -> 'b syntax -> ('a * 'b) syntax (* Follow_conflict *)
  val map : ('a -> 'b) -> 'a syntax -> 'b syntax

  val parse : 'a syntax -> token Seq.t -> 'a option

  module Det : Ll1.S
    with type token = token
    with type token_set = token_set

  val compile : 'a syntax -> 'a Det.syntax

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
        T.union
          (if is_nullable s1 then first s2 else T.empty)
          (if is_productive s2 then first s1 else T.empty)
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
        T.union
          (* If the trailing syntax is nullable, take the should-not-follow set
            from the preceding syntax *)
          (if is_nullable s2 then should_not_follow s1 else T.empty)
          (* If the preceding syntax has a chance of succeeding, then take the
            should-not-follow set of the trailing syntax *)
          (if is_productive s1 then should_not_follow s2 else T.empty)
    | Map (_, s) ->
        should_not_follow s

  exception Nullable_conflict
  exception First_conflict
  exception Follow_conflict

  let elem t = Elem t
  let fail = Fail
  let pure x = Pure x

  let alt s1 s2 =
    (* There should only be one nullable branch *)
    if is_nullable s1 && is_nullable s2 then
      raise Nullable_conflict;

    (* The branches of the alternation must be disjoint *)
    if not (T.disjoint (first s1) (first s2)) then
      raise First_conflict;

    Alt (s1, s2)

  let seq s1 s2 =
    (* The first set of the trailing syntax should not contain any element
      from the should-not-follow set of the preceding syntax. s *)
    if not (T.disjoint (should_not_follow s1) (first s2)) then
      raise Follow_conflict;

    Seq (s1, s2)

  let map f s = Map (f, s)

  (** Returns the state of the syntax after seeing a token. This operation is
      {i not} tail-recursive, and the resulting derivative can grow larger than
      the original syntax. *)
  let rec derive : type a. a syntax -> token -> a syntax option =
    let open Option.Notation in
    fun s t ->
      match s with
      | Elem tk when T.mem t tk ->
          Some (Pure t)
      | Elem _ -> None
      | Fail -> None
      | Pure _ -> None
      | Alt (s1, s2) ->
          begin match T.mem t (first s1) with
          | true -> derive s1 t
          | false -> derive s2 t
          end
      | Seq (s1, s2) ->
          begin match nullable s1 with
          | Some x when T.mem t (first s2) ->
              let* s2' = derive s2 t in
              Some (Seq (Pure x, s2'))
          | Some _ | None ->
              let* s1' = derive s1 t in
              Some (Seq (s1', s2))
          end
      | Map (f, s) ->
          let+ s' = derive s t in
          Map (f, s')

  let rec parse : type a. a syntax -> token Seq.t -> a option =
    let open Option.Notation in
    fun s ts ->
      match Seq.uncons ts with
      | None -> nullable s
      | Some (t, ts) ->
          if T.mem t (first s) then
            let* s' = derive s t in
            (parse [@tailcall]) s' ts
          else
            None

  (** Deterministic syntax descriptions *)
  module Det = Ll1.Make (T)

  (** Compile to a deterministic syntax description, avoiding the need compute
      the derivative at runtime. *)
  let rec compile : type a. a syntax -> a Det.syntax =
    fun s ->
      Det.syntax (nullable s) (compile_branches s)

  and compile_branches : type a. a syntax -> (token_set * a Det.syntax_k) list =
    function
    | Elem tk -> [(tk, Det.elem)]
    | Fail -> []
    | Pure _ -> []
    | Alt (s1, s2) ->
        compile_branches s1 @ compile_branches s2
    | Seq (s1, s2) ->
        let branches1 =
          match nullable s1 with
          | Some x -> compile_branches s2 |> List.map (fun (tk, s2) -> (tk, Det.seq1 x s2))
          | None -> []
        and branches2 =
          (* TODO: Adding a join-point would avoid duplication in the generated code *)
          let s2 = compile s2 in
          compile_branches s1 |> List.map (fun (tk, s1) -> (tk, Det.seq2 s1 s2))
        in
        branches1 @ branches2
    | Map (f, s) ->
        compile_branches s |> List.map (fun (tk, s) -> (tk, Det.map f s))

end

module Char = Make (Set.Char)
