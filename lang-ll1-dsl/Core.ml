module FormatInfo = struct
  (** Semantic information about a binary format. *)

  type t = {
    nullable : bool;
    (** The {i nullability predicate}, i.e. whether the parser might succeed
        while consuming no input. *)

    first : ByteSet.t;
    (** The {i first set}, i.e. the set of bytes that can appear as the first
        byte of this parser. *)

    follow_last : ByteSet.t;
    (** The {i follow set}, i.e. the set of bytes that can appear at the first
        byte of each suffix. *)
  }

  let empty = {
    nullable = true; (* Never consumes any input *)
    first = ByteSet.empty;
    follow_last = ByteSet.empty;
  }

  let byte s = {
    nullable = false; (* Always consumes exactly one byte from the input *)
    first = s;
    follow_last = ByteSet.empty;
  }

  (** [separate i1 i2] checks that the follow set of [i1] type does not
      overlap with the first set of [i1]. This is important to ensure that we
      know for certain when to stop parsing a parser with type [i1], and to
      start parsing a parser of type [i2] without needing to backtrack. *)
  let separate i1 i2 =
    (* TODO: Could it be ok for either [i1] or [i2] to be nullable? *)
    not i1.nullable && ByteSet.disjoint i1.follow_last i2.first

  (** [non_overlapping i1 i2] checks if the two types can be uniquely
      distinguished based on their first sets. This is important to avoid
      ambiguities in alternation and hence avoid backtracking. *)
  let non_overlapping i1 i2 =
    not (i1.nullable && i2.nullable) && ByteSet.disjoint i1.first i2.first

  let cat i1 i2 = {
    nullable = false;
    first = i1.first;
    follow_last =
      ByteSet.union
        i2.follow_last
        (if i2.nullable
          then ByteSet.union i2.first i1.follow_last
          else ByteSet.empty);
  }

  let alt i1 i2 = {
    nullable = i1.nullable || i2.nullable;
    first = ByteSet.union i1.first i2.first;
    follow_last = ByteSet.union i1.follow_last i2.follow_last;
  }

end


type ty =
  | UnitTy
  | ByteTy
  | PairTy of ty * ty

type expr =
  | Local of int
  | Ann of expr * ty
  | UnitIntro
  | ByteIntro of char
  | PairIntro of expr * expr

type format_node =
  | Item of string
  | Empty
  | Byte of ByteSet.t
  | Cat of format * format
  | Alt of format * format
  | Map of ty * (string * expr) * format
and format = {
  node : format_node;
  repr : ty;
  info : FormatInfo.t;
}

type program = {
  items : (string * format) list;
}


(* Pretty printing *)

let rec pp_print_ty ppf t =
  match t with
  | PairTy (t0, t1) ->
      Format.fprintf ppf "Pair@ %a@ %a"
        pp_print_atomic_ty t0
        pp_print_atomic_ty t1
  | t -> pp_print_atomic_ty ppf t

and pp_print_atomic_ty ppf t =
  match t with
  | UnitTy -> Format.fprintf ppf "Unit"
  | ByteTy -> Format.fprintf ppf "Byte"
  | t -> Format.fprintf ppf "(%a)" pp_print_ty t

let rec pp_print_expr names ppf e =
  match e with
  | PairIntro (e0, e1) ->
      Format.fprintf ppf "%a,@ %a"
        (pp_print_atomic_expr names) e0
        (pp_print_expr names) e1
  | e -> pp_print_atomic_expr names ppf e

and pp_print_atomic_expr names ppf e =
  match e with
  | Local index -> Format.pp_print_string ppf (List.nth names index)
  | UnitIntro -> Format.fprintf ppf "()"
  | ByteIntro c -> Format.fprintf ppf "%i" (Char.code c)
  | e -> Format.fprintf ppf "(%a)" (pp_print_atomic_expr names) e

let rec pp_print_format ppf f =
  pp_print_alt_format ppf f

and pp_print_alt_format ppf f =
  match f.node with
  | Alt (f0, f1) ->
      Format.fprintf ppf "@[%a@]@ |@ %a"
        pp_print_cat_format f0
        pp_print_alt_format f1
  | _ -> Format.fprintf ppf "%a" pp_print_cat_format f

and pp_print_cat_format ppf f =
  match f.node with
  | Cat (f0, f1) ->
      Format.fprintf ppf "@[%a,@]@ %a"
        pp_print_range_format f0
        pp_print_cat_format f1
  | _ -> Format.fprintf ppf "%a" pp_print_range_format f

and pp_print_range_format ppf f =
  match f.node with
  | Map (t, (n, e), f) ->
      Format.fprintf ppf "@[map@ @@%a@ (%s@ =>@ %a)@ %a@]"
        pp_print_atomic_ty t
        n
        (pp_print_expr [n]) e
        pp_print_atomic_format f
  | _ -> Format.fprintf ppf "%a" pp_print_atomic_format f

and pp_print_atomic_format ppf f =
  match f.node with
  | Item name -> Format.fprintf ppf "%s" name
  | Empty -> Format.fprintf ppf "()"
  | Byte s -> Format.fprintf ppf "@[%a@]" ByteSet.pp_print s (* TODO: Custom printing *)
  | _ -> Format.fprintf ppf "(%a)" pp_print_format f

let pp_print_program ppf p =
  let rec go ppf items =
    match items with
    | [] -> Format.fprintf ppf ""
    | (name, format) :: items ->
        Format.fprintf ppf "@[<2>@[def@ %s@ :@ Format@ :=@]@ @[%a;@]@]"
          name
          pp_print_format format;
        Format.pp_force_newline ppf ();
        Format.pp_force_newline ppf ();
        (go [@tailcall]) ppf items
  in
  go ppf p.items


module Semantics = struct

  type vexpr =
    | UnitIntro
    | ByteIntro of char
    | PairIntro of vexpr * vexpr

  type local_env =
    vexpr list

  let rec eval (locals : local_env) (e : expr) : vexpr =
    match e with
    | Local x -> begin
        match List.nth_opt locals x with
        | Some v -> v
        | None -> invalid_arg "unbound local variable"
    end
    | Ann (e, _) -> eval locals e
    | UnitIntro -> UnitIntro
    | ByteIntro c -> ByteIntro c
    | PairIntro (e0, e1) -> PairIntro (eval locals e0, eval locals e1)

  let rec quote (ev : vexpr) : expr =
    match ev with
    | UnitIntro -> UnitIntro
    | ByteIntro c -> ByteIntro c
    | PairIntro (e0, e1) -> PairIntro (quote e0, quote e1)

  let normalise (locals : local_env) (e : expr) : expr =
    quote (eval locals e)


  (* Decode semantics *)

  exception DecodeFailure of int

  let get_byte input pos =
    if pos < Bytes.length input then
      Some (Bytes.unsafe_get input pos)
    else
      None

  let rec decode p f input pos : int * vexpr =
    match f.node with
    | Item name -> begin
        match List.assoc_opt name p.items with
        | Some f -> decode p f input pos
        | None -> invalid_arg "unbound item variable"
    end
    | Empty -> pos, UnitIntro
    | Byte s -> begin
        match get_byte input pos with
        | Some c when ByteSet.mem c s -> pos + 1, ByteIntro c
        | _ -> raise (DecodeFailure pos)
    end
    | Cat (f0, f1) ->
        let pos, e0 = decode p f0 input pos in
        let pos, e1 = decode p f1 input pos in
        pos, PairIntro (e0, e1)
    | Alt (f0, f1) -> begin
        match get_byte input pos with
        | Some b when ByteSet.mem b f0.info.first -> decode p f0 input pos
        | Some b when ByteSet.mem b f1.info.first -> decode p f1 input pos
        | _ when f0.info.nullable -> decode p f0 input pos
        | _ when f1.info.nullable -> decode p f1 input pos
        | _ -> raise (DecodeFailure pos)
    end
    | Map (_, (_, e), f) ->
        let pos, ev = decode p f input pos in
        pos, eval [ev] e

end


module Refiner = struct

  module Void = Basis.Void


  (* Contexts *)

  type item_context = (string * (ty * FormatInfo.t)) list
  type local_context = ty list


  (* Context monads *)

  type ('a, 'e) item_m = item_context -> ('a, 'e) result
  type ('a, 'e) local_m = item_context -> local_context -> ('a, 'e) result

  module ItemM = struct

    type ('a, 'e) m = ('a, 'e) item_m

    let pure (x : 'a) : ('a, 'e) m =
      fun _ ->
        Ok x

    let bind (m : ('a, 'e) m) (f : 'a -> ('b, 'e) m) : ('b, 'e) m =
      fun items ->
        Result.bind (m items)
          (fun x -> f x items)

    let lookup_item (n : string) : ((ty * FormatInfo.t) option, 'e) m =
      fun items ->
        Ok (List.assoc_opt n items)

    let handle (handler : 'e_in -> ('a, 'e_out) m) (m : ('a, 'e_in) m) : ('a, 'e_out) m =
      fun items ->
        match m items with
        | Ok x -> Ok x
        | Error e -> (handler e) items

    let throw (e : 'e) : ('a, 'e) m =
      fun _ ->
        Error e

    let local_m (locals : local_context) (m : ('a, 'e) local_m) : ('a, 'e) m =
      fun items ->
        m items locals

    let run (m : ('a, 'e) m) : ('a, 'e) result =
      m []

  end

  module LocalM = struct

    type ('a, 'e) m = ('a, 'e) local_m

    let pure (x : 'a) : ('a, 'e) m =
      fun _ _ ->
        Ok x

    let bind (m : ('a, 'e) m) (f : 'a -> ('b, 'e) m) : ('b, 'e) m =
      fun items locals ->
        Result.bind (m items locals)
          (fun x -> f x items locals)

    let lookup_local (level : int) : ((expr * ty) option, 'e) m =
      fun _ locals ->
        let index = List.length locals - level - 1 in
        Ok (List.nth_opt locals index |> Option.map (fun t -> Local index, t))

    let handle (handler : 'e_in -> ('a, 'e_out) m) (m : ('a, 'e_in) m) : ('a, 'e_out) m =
      fun items locals ->
        match m items locals with
        | Ok x -> Ok x
        | Error e -> (handler e) items locals

    let throw (e : 'e) : ('a, 'e) m =
      fun _ _ ->
        Error e

    let item_m (m : ('a, 'e) item_m) : ('a, 'e) m =
      fun items _ ->
        m items

  end


  (* Forms of judgement *)

  type local_var = {
    level : int;
  }

  type item_var = {
    name : string;
  }

  type 'e is_format = (format, 'e) ItemM.m
  type 'e is_program = (program, 'e) ItemM.m
  type 'e is_ty = (ty, 'e) ItemM.m
  type 'e synth_ty = (expr * ty, 'e) LocalM.m
  type 'e check_ty = ty -> (expr, 'e) LocalM.m


  (* Inference rules *)

  module Program = struct

    let ( let* ) = Result.bind

    let empty : 'e is_program =
      ItemM.pure { items = [] }

    let def_format (name, f) (body : item_var -> 'e is_program) : 'e is_program =
      fun items ->
        let* f = f items in
        let* program = body { name } ((name, (f.repr, f.info)) :: items) in
        Ok { items = (name, f) :: program.items }

  end

  module Format = struct

    let ( let* ) = ItemM.bind

    let empty : 'e is_format =
      ItemM.pure {
        node = Empty;
        repr = UnitTy;
        info = FormatInfo.empty;
      }

    let item (var : item_var) : 'e is_format =
      let* item = ItemM.lookup_item var.name in
      match item with
      | Some (repr, info) -> ItemM.pure { node = Item var.name; repr; info }
      | None -> invalid_arg "unbound item variable"

    let byte (s : ByteSet.t) : 'e is_format =
      ItemM.pure {
        node = Byte s;
        repr = ByteTy;
        info = FormatInfo.byte s;
      }

    let cat (f0 : Void.t is_format) (f1 : Void.t is_format) : [`AmbiguousFormat] is_format =
      let* f0 = f0 |> ItemM.handle Void.absurd in
      let* f1 = f1 |> ItemM.handle Void.absurd in
      if not (FormatInfo.separate f0.info f1.info) then
        ItemM.throw `AmbiguousFormat
      else
        ItemM.pure {
          node = Cat (f0, f1);
          repr = PairTy (f0.repr, f1.repr);
          info = FormatInfo.cat f0.info f1.info;
        }

    let alt (f0 : Void.t is_format) (f1 : Void.t is_format) : [`AmbiguousFormat | `ReprMismatch of ty * ty] is_format =
      let* f0 = f0 |> ItemM.handle Void.absurd in
      let* f1 = f1 |> ItemM.handle Void.absurd in
      if not (FormatInfo.non_overlapping f0.info f1.info) then
        ItemM.throw `AmbiguousFormat
      else if f0.repr <> f1.repr then
        ItemM.throw (`ReprMismatch (f0.repr, f1.repr))
      else
        ItemM.pure {
          node = Alt (f0, f1);
          repr = f0.repr;
          info = FormatInfo.alt f0.info f1.info;
        }

    let map (x, e : string * (local_var -> 'e synth_ty)) (f : 'e is_format) : 'e is_format =
      let* f = f in
      let* e, t = ItemM.local_m [f.repr] (e { level = 0 }) in
      ItemM.pure {
        node = Map (t, (x, e), f);
        repr = t;
        info = f.info;
      }

  end

  module Structural = struct

    let ( let* ) = LocalM.bind

    let local (var : local_var) : 'e synth_ty =
      let* binding = LocalM.lookup_local var.level in
      match binding with
      | Some (e, t) -> LocalM.pure (e, t)
      | None -> invalid_arg "unbound local variable"

    let conv (e : Void.t synth_ty) : [`TypeMismatch of ty * ty] check_ty =
      fun t ->
        let* e, t' = e |> LocalM.handle Void.absurd in
        if t = t' then LocalM.pure e else
          LocalM.throw (`TypeMismatch (t, t'))

    let ann (e : 'e check_ty) (t : 'e is_ty) : 'e synth_ty =
      let* t = LocalM.item_m t in
      let* e = e t in
      LocalM.pure (Ann (e, t), t)

  end

  module Unit = struct

    let form : 'e is_ty =
      ItemM.pure UnitTy

    let intro : 'e synth_ty =
      LocalM.pure (UnitIntro, UnitTy)

  end

  module Byte = struct

    let form : 'e is_ty =
      ItemM.pure ByteTy

    let intro c : 'e synth_ty =
      LocalM.pure (ByteIntro c, ByteTy)

  end

  module Pair = struct

    let ( let* ) = ItemM.bind

    let form (t0 : 'e is_ty) (t1 : 'e is_ty) : 'e is_ty =
      let* t0 = t0 in
      let* t1 = t1 in
      ItemM.pure (PairTy (t0, t1))

    let ( let* ) = LocalM.bind

    let intro (e0 : 'e synth_ty) (e1 : 'e synth_ty) : 'e synth_ty =
      let* e0, t0 = e0 in
      let* e1, t1 = e1 in
      LocalM.pure (PairIntro (e0, e1), PairTy (t0, t1))

  end

end
