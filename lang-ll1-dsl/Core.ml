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

  type item_context = (string * (ty * FormatInfo.t)) list
  type local_context = ty list

  type local_var = {
    level : int;
  }

  type item_var = {
    name : string;
  }

  type is_format = item_context -> format
  type is_program = item_context -> program
  type synth_ty = item_context -> local_context -> expr * ty
  type check_ty = item_context -> local_context -> ty -> expr


  let run_is_program (p : is_program) : program =
    p []


  (* Inference rules *)

  module Program = struct

    let empty : is_program =
      fun _ ->
        { items = [] }

    let def_format (name, f) (body : item_var -> is_program) : is_program =
      fun items ->
        let f = f items in
        let program = body { name } ((name, (f.repr, f.info)) :: items) in
        { items = (name, f) :: program.items }

  end

  module Format = struct

    exception AmbiguousConcatenation
    exception AmbiguousAlternation

    let empty : is_format =
      fun _ ->
        { node = Empty;
          repr = UnitTy;
          info = FormatInfo.empty;
        }

    let item (var : item_var) : is_format =
      fun items ->
        match List.assoc_opt var.name items with
        | Some (repr, info) -> { node = Item var.name; repr; info }
        | None -> invalid_arg "unbound item variable"

    let byte (s : ByteSet.t) : is_format =
      fun _ ->
        { node = Byte s;
          repr = ByteTy;
          info = FormatInfo.byte s;
        }

    let cat (f0 : is_format) (f1 : is_format) : is_format =
      fun items ->
        let f0 = f0 items in
        let f1 = f1 items in
        if FormatInfo.separate f0.info f1.info then
          { node = Cat (f0, f1);
            repr = PairTy (f0.repr, f1.repr);
            info = FormatInfo.cat f0.info f1.info;
          }
        else
          raise AmbiguousConcatenation

    let alt (f0 : is_format) (f1 : is_format) : is_format =
      fun items ->
        let f0 = f0 items in
        let f1 = f1 items in
        (* TODO: Separate type mismatch error *)
        if FormatInfo.non_overlapping f0.info f1.info && f0.repr = f1.repr then
          { node = Alt (f0, f1);
            repr = f0.repr;
            info = FormatInfo.alt f0.info f1.info;
          }
        else
          raise AmbiguousAlternation

    let map (x, e : string * (local_var -> synth_ty)) (f : is_format) : is_format =
      fun items ->
        let f = f items in
        let e, t = e { level = 0 } items [f.repr] in
        { node = Map (t, (x, e), f);
          repr = t;
          info = f.info;
        }

  end

  module Structural = struct

    let local (var : local_var) : synth_ty =
      fun _ locals ->
        let index = List.length locals - var.level - 1 in
        match List.nth_opt locals index with
        | Some ty -> (Local index, ty)
        | None -> invalid_arg "unbound local variable"

  end

  module Unit = struct

    let intro : synth_ty =
      fun _ _ ->
        UnitIntro, UnitTy

  end

  module Byte = struct

    let intro c : synth_ty =
      fun _ _ ->
        ByteIntro c, ByteTy

  end

  module Pair = struct

    let intro (e0 : synth_ty) (e1 : synth_ty) : synth_ty =
      fun items locals ->
        let e0, t0 = e0 items locals in
        let e1, t1 = e1 items locals in
        PairIntro (e0, e1), PairTy (t0, t1)

  end

end
