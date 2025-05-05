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

  module Det : sig

    type _ syntax

    val parse : 'a syntax -> token Seq.t -> ('a * token Seq.t) option

  end

  val compile : 'a syntax -> 'a Det.syntax

end

module Make (T : Set.S) : S
  with type token = T.elt
  with type token_set = T.t
= struct

  type token = T.elt
  type token_set = T.t

  type 'a syntax = {
    data : 'a syntax_data;
    nullable : 'a option;
    is_productive : bool;
    first : token_set;
    should_not_follow : token_set;
    (* id : int; *)
    (* visited : int list; *)
  }

  and 'a syntax_data =
    | Elem : token_set -> token syntax_data
    | Fail : 'a syntax_data
    | Pure : 'a -> 'a syntax_data
    | Alt : 'a syntax * 'a syntax -> 'a syntax_data
    | Seq : 'a syntax * 'b syntax -> ('a * 'b) syntax_data
    | Map : ('a -> 'b) * 'a syntax -> 'b syntax_data
    (* TODO: variables *)

  exception Nullable_conflict
  exception First_conflict
  exception Follow_conflict

  let elem (tk : token_set) : token syntax = {
    data = Elem tk;
    nullable = None;
    is_productive = true;
    first = tk;
    should_not_follow = T.empty;
  }

  let fail (type a) : a syntax = {
    data = Fail;
    nullable = None;
    is_productive = false;
    first = T.empty;
    should_not_follow = T.empty;
  }

  let pure (type a) (x : a) : a syntax = {
    data = Pure x;
    nullable = Some x;
    is_productive = true;
    first = T.empty;
    should_not_follow = T.empty;
  }

  let alt (type a) (s1 : a syntax) (s2 : a syntax) : a syntax =
    (* There should only be one nullable branch *)
    if Option.is_some s1.nullable && Option.is_some s2.nullable then
      raise Nullable_conflict;

    (* The branches of the alternation must be disjoint *)
    if not (T.disjoint s1.first s2.first) then
      raise First_conflict;

    {
      data = Alt (s1, s2);
      nullable = Option.alt s1.nullable (fun () -> s2.nullable);
      is_productive = s1.is_productive || s2.is_productive;
      first = T.union s1.first s2.first;

      should_not_follow =
        T.union s1.should_not_follow s2.should_not_follow
        (* Elements of the should-not-follow set are introduced below *)
        |> T.union (if Option.is_some s2.nullable then s1.first else T.empty)
        |> T.union (if Option.is_some s1.nullable then s2.first else T.empty);
    }

  let seq (type a b) (s1 : a syntax) (s2 : b syntax) : (a * b) syntax =
    (* The first set of the trailing syntax should not contain any element
      from the should-not-follow set of the preceding syntax. s *)
    if not (T.disjoint s1.should_not_follow s2.first) then
      raise Follow_conflict;

    {
      data = Seq (s1, s2);
      nullable = Option.both s1.nullable s2.nullable;
      is_productive = s1.is_productive && s2.is_productive;

      first =
        T.union
          (if Option.is_some s1.nullable then s2.first else T.empty)
          (if s2.is_productive then s1.first else T.empty);

      should_not_follow =
        T.union
          (* If the trailing syntax is nullable, take the should-not-follow set
            from the preceding syntax *)
          (if Option.is_some s2.nullable then s1.should_not_follow else T.empty)
          (* If the preceding syntax has a chance of succeeding, then take the
            should-not-follow set of the trailing syntax *)
          (if s1.is_productive then s2.should_not_follow else T.empty);
    }

  let map (type a b) (f : a -> b) (s : a syntax) : b syntax = {
    data = Map (f, s);
    nullable = s.nullable |> Option.map f;
    is_productive = s.is_productive;
    first = s.first;
    should_not_follow = s.should_not_follow;
  }

  (** Returns the state of the syntax after seeing a token. This operation is
      {i not} tail-recursive, and the resulting derivative can grow larger than
      the original syntax. *)
  let rec derive : type a. a syntax -> token -> a syntax option =
    let open Option.Notation in
    fun s t ->
      match s.data with
      | Elem tk when T.mem t tk ->
          Some (pure t)
      | Elem _ -> None
      | Fail -> None
      | Pure _ -> None
      | Alt (s1, s2) ->
          begin match T.mem t s1.first with
          | true -> derive s1 t
          | false -> derive s2 t
          end
      | Seq (s1, s2) ->
          begin match s1.nullable with
          | Some x when T.mem t (s2.first) ->
              let* s2' = derive s2 t in
              Some (seq (pure x) s2')
          | Some _ | None ->
              let* s1' = derive s1 t in
              Some (seq s1' s2)
          end
      | Map (f, s) ->
          let+ s' = derive s t in
          map f s'

    let rec parse : type a. a syntax -> token Seq.t -> a option =
      let open Option.Notation in
      fun s ts ->
        match Seq.uncons ts with
        | None -> s.nullable
        | Some (t, ts) ->
            if T.mem t s.first then
              let* s' = derive s t in
              (parse [@tailcall]) s' ts
            else
              None

  (** Deterministic syntax descriptions.

      These can be parsed with a single token of lookahead.
  *)
  module Det = struct

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

    let rec parse : type a. a syntax -> token Seq.t -> (a * token Seq.t) option =
      (* TODO: It would be nice if we could make this function return [a option]
        instead of [(a * token Seq.t) option], in a similar fashion to the
        derivative-based parser. *)
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

  end

  (** Compile to a deterministic syntax description, avoiding the need compute
      the derivative at runtime. *)
  let rec compile : type a. a syntax -> a Det.syntax =
    fun s -> {
      pure = s.nullable;
      alt = compile_branches s;
    }

  and compile_branches : type a. a syntax -> (token_set * a Det.syntax_k) list =
    fun s ->
      match s.data with
      | Elem tk -> [(tk, Elem)]
      | Fail -> []
      | Pure _ -> []
      | Alt (s1, s2) ->
          compile_branches s1 @ compile_branches s2
      | Seq (s1, s2) ->
          let branches1 =
            match s1.nullable with
            | Some x ->
                compile_branches s2
                |> List.map Det.(fun (tk, s2) -> (tk, Seq1 (x, s2)))
            | None -> []
          and branches2 =
            (* TODO: Adding a join-point would avoid duplication in the generated code *)
            let s2 = compile s2 in
            compile_branches s1
            |> List.map Det.(fun (tk, s1) -> (tk, Seq2 (s1, s2)))
          in
          branches1 @ branches2
      | Map (f, s) ->
          compile_branches s
          |> List.map Det.(fun (tk, s) -> (tk, Map (f, s)))

end

module Char = Make (Set.Char)
