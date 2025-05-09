(** Syntax descriptions that are guaranteed to be LL(1) when they are
    constructed. They can either be parsed with derivatives or compiled to
    {!Ll1} syntax descriptions for improved performance.
*)

module type S = sig

  type token
  type token_set

  type _ t

  exception Nullable_conflict
  exception First_conflict
  exception Follow_conflict

  (** Core syntax descriptions *)

  val token : token_set -> token t
  val fail : 'a t
  val pure : 'a -> 'a t
  val alt : 'a t -> 'a t -> 'a t (* Nullable_conflict, First_conflict *)
  val seq : 'a t -> 'b t -> ('a * 'b) t (* Follow_conflict *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** Derived syntax descriptions *)

  val app : ('a -> 'b) t -> 'a t -> 'b t
  val opt : 'a t -> 'a -> 'a t
  val alts : 'a t list -> 'a t
  val list : 'a t list -> 'a list t

  module Notation : sig

    val ( <|> ) : 'a t -> 'a t -> 'a t
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
    val ( let+ ) : ('a -> 'b) -> 'a t -> 'b t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  end

  (** Parsing with derivatives *)

  val parse : 'a t -> token Seq.t -> 'a option

  (** Compiling to case trees *)

  module Case_tree : Ll1_case_tree.S
    with type token = token
    with type token_set = token_set

  val compile : 'a t -> 'a Case_tree.t

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
      {!node}, but because we use them multiple times, it’s probably
      more efficient to compute them eagerly and store them for later use as
      we build the syntax descriptions.
  *)
  type 'a t = {
    node : 'a node;
    properties : 'a Properties.t;
  }

  (** Syntax descriptions *)
  and 'a node =
    | Token : token_set -> token node
    | Fail : 'a node
    | Pure : 'a -> 'a node
    | Alt : 'a t * 'a t -> 'a node
    | Seq : 'a t * 'b t -> ('a * 'b) node
    | Map : ('a -> 'b) * 'a t -> 'b node
    (* TODO: variables *)

  let nullable s = s.properties |> Properties.nullable
  let first s = s.properties |> Properties.first

  exception Nullable_conflict = Properties.Nullable_conflict
  exception First_conflict = Properties.First_conflict
  exception Follow_conflict = Properties.Follow_conflict

  let token (tk : token_set) : token t = {
    node = Token tk;
    properties = Properties.token tk;
  }

  let fail (type a) : a t = {
    node = Fail;
    properties = Properties.fail;
  }

  let pure (type a) (x : a) : a t = {
    node = Pure x;
    properties = Properties.pure x;
  }

  let alt (type a) (s1 : a t) (s2 : a t) : a t = {
    node = Alt (s1, s2);
    properties = Properties.alt s1.properties s2.properties;
  }

  let seq (type a b) (s1 : a t) (s2 : b t) : (a * b) t = {
    node = Seq (s1, s2);
    properties = Properties.seq s1.properties s2.properties;
  }

  let map (type a b) (f : a -> b) (s : a t) : b t = {
    node = Map (f, s);
    properties = Properties.map f s.properties;
  }

  (** Derived syntax descriptions *)

  let app (type a b) (s1 : (a -> b) t) (s2 : a t) : b t =
    seq s1 s2 |> map (fun (f, x) -> f x)

  let opt (type a) (s : a t) (x : a) : a t =
    alt s (pure x)

  let rec alts : type a. a t list -> a t =
    function
    | [] -> fail
    | [s] -> s
    | s :: ss -> alt s (alts ss)

  let rec list : type a. a t list -> a list t =
    function
    | [] -> pure []
    | s :: ss -> seq s (list ss) |> map (fun (x, xs) -> x :: xs)

  module Notation = struct

    let ( <|> ) = alt
    let ( <$> ) = map
    let ( <*> ) = app
    let ( let+ ) = map
    let ( and+ ) = seq

  end

  (** Returns the state of the syntax after seeing a token. This operation is
      {i not} tail-recursive, and the resulting derivative can grow larger than
      the original syntax. *)
  let rec derive : type a. a t -> token -> a t option =
    let open Option.Notation in
    (* TODO: Avoid recomputing LL(1) properties over and over? *)
    fun s t ->
      match s.node with
      | Token tk when T.mem t tk ->
          Some (pure t)
      | Token _ -> None
      | Fail -> None
      | Pure _ -> None
      | Alt (s1, s2) ->
          begin match T.mem t (first s1) with
          | true -> derive s1 t
          | false -> derive s2 t
          end
      | Seq (s1, s2) ->
          begin match (nullable s1) with
          | Some x when T.mem t (first s2) ->
              let* s2' = derive s2 t in
              Some (seq (pure x) s2')
          | Some _ | None ->
              let* s1' = derive s1 t in
              Some (seq s1' s2)
          end
      | Map (f, s) ->
          let+ s' = derive s t in
          map f s'

  let rec parse : type a. a t -> token Seq.t -> a option =
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
  module Case_tree = Ll1_case_tree.Make (T)

  (** Compile to a deterministic syntax description, avoiding the need compute
      the derivative at runtime. *)
  let rec compile : type a. a t -> a Case_tree.t =
    fun s ->
      Case_tree.make (nullable s) (compile_cases s)

  and compile_cases : type a. a t -> (token_set * a Case_tree.cont) list =
    fun s ->
      match s.node with
      | Token tk -> [(tk, Case_tree.token)]
      | Fail -> []
      | Pure _ -> []
      | Alt (s1, s2) ->
          compile_cases s1 @ compile_cases s2
      | Seq (s1, s2) ->
          let branches1 =
            match (nullable s1) with
            | Some x -> compile_cases s2 |> List.map (fun (tk, s2) -> (tk, Case_tree.seq1 x s2))
            | None -> []
          and branches2 =
            (* TODO: Adding a join-point would avoid duplication in the generated code *)
            let s2 = compile s2 in
            compile_cases s1 |> List.map (fun (tk, s1) -> (tk, Case_tree.seq2 s1 s2))
          in
          branches1 @ branches2
      | Map (f, s) ->
          compile_cases s |> List.map (fun (tk, s) -> (tk, Case_tree.map f s))

end

module Char = Make (Set.Char)
