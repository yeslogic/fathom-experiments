(** Syntax descriptions that are guaranteed to be LL(1) when they are
    constructed. These syntax descriptions can either be parsed with derivatives
    or compiled to {!Ll1_det} syntax descriptions for improved performance.

    The implementation is very similar to the one used in {!LL1_simple}, but we
    precompute the LL(1) properties of syntax descriptions ahead of time,
    storing them in a mutually defined record in  order to avoid recomputing
    them multiple times during parsing or compilation. Note that the
    implementation of {{: https://doi.org/10.1145/3385412.3385992} “Zippy LL(1)
    parsing with derivatives”} uses {i propagator networks} to account for
    cyclic syntaxes arising from top-level mutual recursion.
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

  module Det : Ll1_det.S
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

  (** LL(1) properties for syntax descriptions *)
  type 'a properties = {
    nullable : 'a option;
    is_productive : bool;
    first : token_set;
    should_not_follow : token_set;
    (* visited : int list; *)
  }

  (** Syntax descriptions along with properties about these descriptions.

      We could have implemented these properties as separate functions on
      {!syntax_data}, but because we use them multiple times, it’s probably
      more efficient to compute them eagerly and store them for later use as
      we build the syntax descriptions.
  *)
  type 'a syntax = {
    data : 'a syntax_data;
    properties : 'a properties;
  }

  (** Syntax descriptions *)
  and 'a syntax_data =
    | Elem : token_set -> token syntax_data
    | Fail : 'a syntax_data
    | Pure : 'a -> 'a syntax_data
    | Alt : 'a syntax * 'a syntax -> 'a syntax_data
    | Seq : 'a syntax * 'b syntax -> ('a * 'b) syntax_data
    | Map : ('a -> 'b) * 'a syntax -> 'b syntax_data
    (* TODO: variables *)

  let nullable s = s.properties.nullable
  let is_nullable s = Option.is_some (nullable s)
  let is_productive s = s.properties.is_productive
  let first s = s.properties.first
  let should_not_follow s = s.properties.should_not_follow

  exception Nullable_conflict
  exception First_conflict
  exception Follow_conflict

  let elem (tk : token_set) : token syntax = {
    data = Elem tk;
    properties = {
      nullable = None;
      is_productive = true;
      first = tk;
      should_not_follow = T.empty;
    };
  }

  let fail (type a) : a syntax = {
    data = Fail;
    properties = {
      nullable = None;
      is_productive = false;
      first = T.empty;
      should_not_follow = T.empty;
    };
  }

  let pure (type a) (x : a) : a syntax = {
    data = Pure x;
    properties = {
      nullable = Some x;
      is_productive = true;
      first = T.empty;
      should_not_follow = T.empty;
    };
  }

  let alt (type a) (s1 : a syntax) (s2 : a syntax) : a syntax =
    (* There should only be one nullable branch *)
    if is_nullable s1 && is_nullable s2 then
      raise Nullable_conflict;

    (* The branches of the alternation must be disjoint *)
    if not (T.disjoint (first s1) (first s2)) then
      raise First_conflict;

    {
      data = Alt (s1, s2);
      properties = {
        nullable = Option.alt (nullable s1) (fun () -> nullable s2);
        is_productive = is_productive s1 || is_productive s2;
        first = T.union (first s1) (first s2);

        should_not_follow =
          T.union (should_not_follow s1) (should_not_follow s2)
          (* Elements of the should-not-follow set are introduced below *)
          |> T.union (if is_nullable s2 then first s1 else T.empty)
          |> T.union (if is_nullable s1 then first s2 else T.empty);
      };
    }

  let seq (type a b) (s1 : a syntax) (s2 : b syntax) : (a * b) syntax =
    (* The first set of the trailing syntax should not contain any element
      from the should-not-follow set of the preceding syntax. s *)
    if not (T.disjoint (should_not_follow s1) (first s2)) then
      raise Follow_conflict;

    {
      data = Seq (s1, s2);
      properties = {
        nullable = Option.both (nullable s1) (nullable s2);
        is_productive = is_productive s1 && is_productive s2;

        first =
          T.union
            (if is_nullable s1 then first s2 else T.empty)
            (if is_productive s2 then first s1 else T.empty);

        should_not_follow =
          T.union
            (* If the trailing syntax is nullable, take the should-not-follow set
              from the preceding syntax *)
            (if is_nullable s2 then should_not_follow s1 else T.empty)
            (* If the preceding syntax has a chance of succeeding, then take the
              should-not-follow set of the trailing syntax *)
            (if is_productive s1 then should_not_follow s2 else T.empty);
      };
    }

  let map (type a b) (f : a -> b) (s : a syntax) : b syntax = {
    data = Map (f, s);
    properties = {
      nullable = nullable s |> Option.map f;
      is_productive = is_productive s;
      first = first s;
      should_not_follow = should_not_follow s;
    };
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
          begin match T.mem t (first s1) with
          | true -> derive s1 t
          | false -> derive s2 t
          end
      | Seq (s1, s2) ->
          begin match (nullable s1) with
          | Some x when T.mem t ((first s2)) ->
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
        | None -> nullable s
        | Some (t, ts) ->
            if T.mem t (first s) then
              let* s' = derive s t in
              (parse [@tailcall]) s' ts
            else
              None

  (** Deterministic syntax descriptions *)
  module Det = Ll1_det.Make (T)

  (** Compile to a deterministic syntax description, avoiding the need compute
      the derivative at runtime. *)
  let rec compile : type a. a syntax -> a Det.syntax =
    fun s ->
      Det.syntax (nullable s) (compile_branches s)

  and compile_branches : type a. a syntax -> (token_set * a Det.syntax_k) list =
    fun s ->
      match s.data with
      | Elem tk -> [(tk, Det.elem)]
      | Fail -> []
      | Pure _ -> []
      | Alt (s1, s2) ->
          compile_branches s1 @ compile_branches s2
      | Seq (s1, s2) ->
          let branches1 =
            match (nullable s1) with
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
