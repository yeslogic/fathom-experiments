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
  | UnitIntro
  | ByteIntro of char
  | PairIntro of expr * expr

type format_node =
  | Item of int
  | Empty
  | Byte of ByteSet.t
  | Cat of format * format
  | Alt of format * format
and format = {
  node : format_node;
  repr : ty;
  info : FormatInfo.t;
}

type program = {
  items : (string * format) list;
}


(* Pretty printing *)

let rec pp_print_format item_names ppf f =
  pp_print_alt_format item_names ppf f

and pp_print_alt_format item_names ppf f =
  match f.node with
  | Alt (f0, f1) ->
      Format.fprintf ppf "@[%a@]@ |@ %a"
        (pp_print_cat_format item_names) f0
        (pp_print_alt_format item_names) f1
  | _ -> Format.fprintf ppf "%a" (pp_print_cat_format item_names) f

and pp_print_cat_format item_names ppf f =
  match f.node with
  | Cat (f0, f1) ->
      Format.fprintf ppf "@[%a,@]@ %a"
        (pp_print_atomic_format item_names) f0
        (pp_print_cat_format item_names) f1
  | _ -> Format.fprintf ppf "%a" (pp_print_atomic_format item_names) f

and pp_print_atomic_format item_names ppf f =
  match f.node with
  | Item level -> Format.fprintf ppf "%s" (List.nth item_names (List.length item_names - level - 1))
  | Empty -> Format.fprintf ppf "()"
  | Byte s -> Format.fprintf ppf "@[%a@]" ByteSet.pp_print s
  | _ -> Format.fprintf ppf "(%a)" (pp_print_format item_names) f

let pp_print_program ppf p =
  let rec go item_names ppf items =
    match items with
    | [] -> Format.fprintf ppf ""
    | (name, format) :: items ->
        Format.fprintf ppf "@[<2>@[def@ %s@ :@ Format@ :=@]@ @[%a;@]@]"
          name
          (pp_print_format item_names) format;
        Format.pp_force_newline ppf ();
        Format.pp_force_newline ppf ();
        (go [@tailcall]) (name:: item_names) ppf items
  in
  go [] ppf p.items


module Refiner = struct

  type item_context = (ty * FormatInfo.t) list

  type item_var = {
    level : int;
  }

  type is_format = item_context -> format
  type is_program = item_context -> program
  type synth_ty = item_context (* TODO: -> local_context *) -> expr * ty
  type check_ty = item_context (* TODO: -> local_context *) -> ty -> expr


  let run_is_program (p : is_program) : program =
    p []


  (* Inference rules *)

  module Program = struct

    let empty : is_program =
      fun _ ->
        { items = [] }

    let def_format (name, f) (body : item_var -> is_program) : is_program =
      fun items ->
        let level = List.length items in
        let f = f items in
        let program = body { level } ((f.repr, f.info) :: items) in
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
        let index = List.length items - var.level - 1 in
        match List.nth_opt items index with
        | Some (repr, info) -> { node = Item var.level; repr; info }
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

  end

  module Unit = struct

    let intro : synth_ty =
      fun _ ->
        UnitIntro, UnitTy

  end

  module Byte = struct

    let intro c : synth_ty =
      fun _ ->
        ByteIntro c, ByteTy

  end

  module Pair = struct

    let intro e0 e1 : synth_ty =
      fun items ->
        let e0, t0 = e0 items in
        let e1, t1 = e1 items in
        PairIntro (e0, e1), PairTy (t0, t1)

  end

end


module Decode = struct

  exception DecodeFailure of int

  let get_byte input pos =
    if pos < Bytes.length input then
      Some (Bytes.unsafe_get input pos)
    else
      None

  let run p f input pos =
    let size = List.length p.items in
    let rec go f pos =
      match f.node with
      | Item level -> begin
          match List.nth_opt p.items (size - level - 1) with
          | Some (_, f) -> go f pos
          | None -> invalid_arg "unbound item variable"
      end
      | Empty -> pos, UnitIntro
      | Byte s -> begin
          match get_byte input pos with
          | Some c when ByteSet.mem c s -> pos + 1, ByteIntro c
          | _ -> raise (DecodeFailure pos)
      end
      | Cat (f0, f1) ->
          let (pos, e0) = go f0 pos in
          let (pos, e1) = go f1 pos in
          pos, PairIntro (e0, e1)
      | Alt (f0, f1) -> begin
          match get_byte input pos with
          | Some b when ByteSet.mem b f0.info.first -> go f0 pos
          | Some b when ByteSet.mem b f1.info.first -> go f1 pos
          | _ when f0.info.nullable -> go f0 pos
          | _ when f1.info.nullable -> go f1 pos
          | _ -> raise (DecodeFailure pos)
      end
    in
    go f pos

end
