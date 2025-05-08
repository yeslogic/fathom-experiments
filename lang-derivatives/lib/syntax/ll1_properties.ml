(** LL(1) properties for syntax descriptions.

    Note that the implementation of {{: https://doi.org/10.1145/3385412.3385992}
    “Zippy LL(1) parsing with derivatives”} uses {i propagator networks} to
    handle top-level mutual recursion.
*)

module type S = sig

  type token
  type token_set

  type _ t

  val nullable : 'a t -> 'a option
  val is_nullable : 'a t -> bool
  val is_productive : 'a t -> bool
  val first : 'a t -> token_set
  val should_not_follow : 'a t -> token_set

  exception Nullable_conflict
  exception First_conflict
  exception Follow_conflict

  val elem : token_set -> token t
  val fail : 'a t
  val pure : 'a -> 'a t
  val alt : 'a t -> 'a t -> 'a t (* Nullable_conflict, First_conflict *)
  val seq : 'a t -> 'b t -> ('a * 'b) t (* Follow_conflict *)
  val map : ('a -> 'b) -> 'a t -> 'b t

end

module Make (T : Set.S) : S
  with type token = T.elt
  with type token_set = T.t
= struct

  type token = T.elt
  type token_set = T.t

  type 'a t = {
    nullable : 'a option;
    is_productive : bool;
    first : token_set;
    should_not_follow : token_set;
    (* visited : int list; *)
  }

  exception Nullable_conflict
  exception First_conflict
  exception Follow_conflict

  let nullable s = s.nullable
  let is_nullable s = Option.is_some (nullable s)
  let is_productive s = s.is_productive
  let first s = s.first
  let should_not_follow s = s.should_not_follow

  let elem (tk : token_set) : token t = {
    nullable = None;
    is_productive = true;
    first = tk;
    should_not_follow = T.empty;
  }

  let fail (type a) : a t = {
    nullable = None;
    is_productive = false;
    first = T.empty;
    should_not_follow = T.empty;
  }

  let pure (type a) (x : a) : a t = {
    nullable = Some x;
    is_productive = true;
    first = T.empty;
    should_not_follow = T.empty;
  }

  let alt (type a) (s1 : a t) (s2 : a t) : a t =
    (* There should only be one nullable branch *)
    if is_nullable s1 && is_nullable s2 then
      raise Nullable_conflict;

    (* The branches of the alternation must be disjoint *)
    if not (T.disjoint (first s1) (first s2)) then
      raise First_conflict;

    {
      nullable = Option.alt (nullable s1) (fun () -> nullable s2);
      is_productive = is_productive s1 || is_productive s2;
      first = T.union (first s1) (first s2);

      should_not_follow =
        T.union (should_not_follow s1) (should_not_follow s2)
        (* Elements of the should-not-follow set are introduced below *)
        |> T.union (if is_nullable s2 then first s1 else T.empty)
        |> T.union (if is_nullable s1 then first s2 else T.empty);
    }

  let seq (type a b) (s1 : a t) (s2 : b t) : (a * b) t =
    (* The first set of the trailing syntax should not contain any element
      from the should-not-follow set of the preceding syntax. s *)
    if not (T.disjoint (should_not_follow s1) (first s2)) then
      raise Follow_conflict;

    {
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
    }

  let map (type a b) (f : a -> b) (s : a t) : b t = {
    nullable = nullable s |> Option.map f;
    is_productive = is_productive s;
    first = first s;
    should_not_follow = should_not_follow s;
  }

end
