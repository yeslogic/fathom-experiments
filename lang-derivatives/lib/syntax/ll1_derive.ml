(** Syntax descriptions that are guaranteed to be LL(1) when they are
    constructed. They can either be parsed with derivatives or compiled to
    {!Ll1} syntax descriptions for improved performance.
*)

module type S = sig

  type token
  type token_set

  type _ syntax

  val nullable : 'a syntax -> 'a option
  val is_nullable : 'a syntax -> bool
  val is_productive : 'a syntax -> bool
  val first : 'a syntax -> token_set
  val should_not_follow : 'a syntax -> token_set

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

  module Properties = Ll1_properties.Make (T)

  type token = T.elt
  type token_set = T.t

  (** Syntax descriptions along with properties about these descriptions.

      We could have implemented these properties as separate functions on
      {!syntax_data}, but because we use them multiple times, it’s probably
      more efficient to compute them eagerly and store them for later use as
      we build the syntax descriptions.
  *)
  type 'a syntax = {
    data : 'a syntax_data;
    properties : 'a Properties.t;
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

  let nullable s = s.properties |> Properties.nullable
  let is_nullable s = s.properties |> Properties.is_nullable
  let is_productive s = s.properties |> Properties.is_productive
  let first s = s.properties |> Properties.first
  let should_not_follow s = s.properties |> Properties.should_not_follow

  exception Nullable_conflict = Properties.Nullable_conflict
  exception First_conflict = Properties.First_conflict
  exception Follow_conflict = Properties.Follow_conflict

  let elem (tk : token_set) : token syntax = {
    data = Elem tk;
    properties = Properties.elem tk;
  }

  let fail (type a) : a syntax = {
    data = Fail;
    properties = Properties.fail;
  }

  let pure (type a) (x : a) : a syntax = {
    data = Pure x;
    properties = Properties.pure x;
  }

  let alt (type a) (s1 : a syntax) (s2 : a syntax) : a syntax = {
    data = Alt (s1, s2);
    properties = Properties.alt s1.properties s2.properties;
  }

  let seq (type a b) (s1 : a syntax) (s2 : b syntax) : (a * b) syntax = {
    data = Seq (s1, s2);
    properties = Properties.seq s1.properties s2.properties;
  }

  let map (type a b) (f : a -> b) (s : a syntax) : b syntax = {
    data = Map (f, s);
    properties = Properties.map f s.properties;
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
  module Det = Ll1.Make (T)

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
