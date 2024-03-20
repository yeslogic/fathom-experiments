module FormatInfo : sig
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


  (** {1 Predicates} *)

  val separate : t -> t -> bool
  (** [separate i1 i2] checks that the follow set of [i1] type does not
      overlap with the first set of [i1]. This is important to ensure that we
      know for certain when to stop parsing a parser with type [i1], and to
      start parsing a parser of type [i2] without needing to backtrack. *)

  val non_overlapping : t -> t -> bool
  (** [non_overlapping i1 i2] checks if the two types can be uniquely
      distinguished based on their first sets. This is important to avoid
      ambiguities in deteministic unions that would require backtracking. *)


  (** {1 Combinators} *)

  val empty : t
  (** [empty] describes a language containing only the empty string. *)

  val byte : ByteSet.t -> t
  (** [byte c] describes a language containg a single byte in the set [s]. *)

  val fail : t
  (** [fail] describes a language with no strings. *)

  val seq : t -> t -> t
  (** [seq i1 i2] describes a language produced by sequencing a language
      of type [i1] before a language of type [i2]. *)

  val union : t -> t -> t
  (** [union i1 i2] describes a language produced from the union of a
      language of type [i1] with a language of type [i2]. *)

end = struct

  type t = {
    nullable : bool;
    first : ByteSet.t;
    follow_last : ByteSet.t;
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

  let fail = byte ByteSet.empty

  let separate i1 i2 =
    (* TODO: Could it be ok for either [i1] or [i2] to be nullable? *)
    not i1.nullable && ByteSet.disjoint i1.follow_last i2.first

  let non_overlapping i1 i2 =
    not (i1.nullable && i2.nullable) && ByteSet.disjoint i1.first i2.first

  let seq i1 i2 = {
    nullable = i1.nullable && i2.nullable;
    first = i1.first;
    follow_last =
      ByteSet.union
        i2.follow_last
        (if i2.nullable
          then ByteSet.union i2.first i1.follow_last
          else ByteSet.empty);
  }

  let union i1 i2 = {
    nullable = i1.nullable || i2.nullable;
    first = ByteSet.union i1.first i2.first;
    follow_last = ByteSet.union i1.follow_last i2.follow_last;
  }

end

(** An unordered row of elements distinguished by label. *)
module LabelMap = Map.Make (String)

[@@@warning "-duplicate-definitions"]

type ty =
  | Item of string
  | ByteTy
  | RecordTy of ty LabelMap.t
  | TupleTy of ty list
  | FormatRepr of format

and expr =
  | Item of string
  | Local of int
  | Ann of expr * ty
  | ByteLit of char
  | RecordLit of expr LabelMap.t
  | RecordProj of expr * string
  | TupleLit of expr list
  | TupleProj of expr * int

and format_node =
  | Item of string
  | Fail of ty
  | Byte of ByteSet.t
  | Seq of format list
  | Union of format * format
  | Pure of ty * expr
  | Map of ty * (string * expr) * format
  | FlatMap of ty * (string * format) * format

and format = {
  node : format_node;
  repr : ty;
  info : FormatInfo.t;
}

type item =
  | Type of ty
  | Format of format
  | Expr of expr * ty

type program = {
  items : (string * item) list;
}


(* Pretty printing *)

let rec pp_print_ty ppf (t : ty) =
  match t with
  | t -> pp_print_atomic_ty ppf t

and pp_print_atomic_ty ppf t =
  match t with
  | Item name -> Format.pp_print_string ppf name
  | ByteTy -> Format.fprintf ppf "Byte"
  | RecordTy fields ->
      Format.fprintf ppf "{@ %a@ }"
        (Format.pp_print_seq
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
          (fun ppf (l, t) -> Format.fprintf ppf "%s@ :@ %a" l pp_print_ty t))
        (LabelMap.to_seq fields)
  | TupleTy ts ->
      Format.fprintf ppf "(%a)"
        (Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
          pp_print_ty)
        ts
  | FormatRepr f ->
      Format.fprintf ppf "%a.Repr"
        (pp_print_atomic_format []) f
  (* | t -> Format.fprintf ppf "(%a)" pp_print_ty t *)

and pp_print_expr names ppf (e : expr) =
  match e with
  | e -> pp_proj_expr names ppf e

and pp_proj_expr names ppf e =
  match e with
  | RecordProj (e, l) -> Format.fprintf ppf "%a.%s" (pp_proj_expr names) e l
  | TupleProj (e, i) -> Format.fprintf ppf "%a.%i" (pp_proj_expr names) e i
  | e -> pp_print_atomic_expr names ppf e

and pp_print_atomic_expr names ppf e =
  match e with
  | Item name -> Format.pp_print_string ppf name
  | Local index -> Format.pp_print_string ppf (List.nth names index)
  | ByteLit c -> Format.fprintf ppf "%i" (Char.code c)
  | RecordLit fields ->
      Format.fprintf ppf "{@ %a@ }"
        (Format.pp_print_seq
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
          (fun ppf (l, f) -> Format.fprintf ppf "%s@ :=@ %a" l (pp_print_expr names) f))
        (LabelMap.to_seq fields)
  | TupleLit es ->
      Format.fprintf ppf "(%a)"
        (Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
          (pp_print_expr names))
        es
  | e -> Format.fprintf ppf "(%a)" (pp_print_atomic_expr names) e

and pp_print_format names ppf (f : format) =
  pp_print_union_format names ppf f

and pp_print_union_format names ppf f =
  match f.node with
  | Union (f0, f1) ->
      Format.fprintf ppf "@[%a@]@ |@ %a"
        (pp_print_app_format names) f0
        (pp_print_union_format names) f1
  | _ ->
      pp_print_app_format names ppf f

and pp_print_app_format names ppf f =
  match f.node with
  | Fail t ->
      Format.fprintf ppf "@[#fail@ @@%a@]"
        pp_print_atomic_ty t
  | Pure (t, e) ->
      Format.fprintf ppf "@[#pure@ @@%a@ %a@]"
        pp_print_atomic_ty t
        (pp_print_expr names) e
  | Map (t, (n, e), f) ->
      Format.fprintf ppf "@[#map@ @@%a@ (%s@ =>@ %a)@ %a@]"
        pp_print_atomic_ty t
        n
        (pp_print_expr (n :: names)) e
        (pp_print_atomic_format names) f
  | FlatMap (t, (n, f1), f0) ->
      Format.fprintf ppf "@[#flat-map@ @@%a@ (%s@ =>@ %a)@ %a@]"
        pp_print_atomic_ty t
        n
        (pp_print_format (n :: names)) f1
        (pp_print_atomic_format names) f0
  | _ ->
      pp_print_atomic_format names ppf f

and pp_print_atomic_format names ppf f =
  match f.node with
  | Item name -> Format.fprintf ppf "%s" name
  | Byte s -> Format.fprintf ppf "@[%a@]" ByteSet.pp_print s (* TODO: Custom printing *)
  | Seq fs ->
      Format.fprintf ppf "(%a)"
        (Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
          (pp_print_format names))
        fs
  | _ -> Format.fprintf ppf "(%a)" (pp_print_format names) f

let pp_print_program ppf p =
  let rec go ppf items =
    match items with
    | [] -> Format.fprintf ppf ""
    | (name, Format f) :: items ->
        Format.fprintf ppf "@[<2>@[def@ %s@ :@ Format@ :=@]@ @[%a;@]@]"
          name
          (pp_print_format []) f;
        Format.pp_force_newline ppf ();
        Format.pp_force_newline ppf ();
        (go [@tailcall]) ppf items
    | (name, Type t) :: items ->
        Format.fprintf ppf "@[<2>@[def@ %s@ :@ Type@ :=@]@ @[%a;@]@]"
          name
          pp_print_ty t;
        Format.pp_force_newline ppf ();
        Format.pp_force_newline ppf ();
        (go [@tailcall]) ppf items
    | (name, Expr (e, t)) :: items ->
        Format.fprintf ppf "@[<2>@[def@ %s@ :@ @[%a@]@ :=@]@ @[%a;@]@]"
          name
          pp_print_ty t
          (pp_print_expr []) e;
        Format.pp_force_newline ppf ();
        Format.pp_force_newline ppf ();
        (go [@tailcall]) ppf items
  in
  go ppf p.items


module Semantics = struct

  type vty =
    | ByteTy
    | RecordTy of vty LabelMap.t
    | TupleTy of vty list

  type vexpr =
    | ByteLit of char
    | RecordLit of vexpr LabelMap.t
    | TupleLit of vexpr list

  type local_env =
    vexpr list


  (* Eliminators *)

  let record_proj (ve : vexpr) (l : string) : vexpr =
    match ve with
    | RecordLit fs -> LabelMap.find l fs
    | _ -> invalid_arg "expected pair"

  let tuple_proj (ve : vexpr) (i : int) : vexpr =
    match ve with
    | TupleLit ves -> List.nth ves i
    | _ -> invalid_arg "expected pair"

  (* Evalution *)

  let rec eval_ty (items : (string * item) list) (t : ty) : vty =
    match t with
    | Item n -> begin
      match List.assoc_opt n items with
      | Some (Type t) -> eval_ty items t
      | Some _ -> invalid_arg "expected type"
      | None -> invalid_arg "unbound item variable"
    end
    | ByteTy -> ByteTy
    | RecordTy fs -> RecordTy (LabelMap.map (eval_ty items) fs)
    | TupleTy ts -> TupleTy (List.map (eval_ty items) ts)
    | FormatRepr f -> eval_ty items f.repr

  let rec eval_expr (items : (string * item) list) (locals : local_env) (e : expr) : vexpr =
    match e with
    | Item n -> begin
      match List.assoc_opt n items with
      | Some (Expr (e, _)) -> eval_expr items [] e
      | Some _ -> invalid_arg "expected expression"
      | None -> invalid_arg "unbound item variable"
    end
    | Local x -> begin
      match List.nth_opt locals x with
      | Some v -> v
      | None -> invalid_arg "unbound local variable"
    end
    | Ann (e, _) -> eval_expr items locals e
    | ByteLit c -> ByteLit c
    | RecordLit fs -> RecordLit (LabelMap.map (eval_expr items locals) fs)
    | RecordProj (e, l) -> record_proj (eval_expr items locals e) l
    | TupleLit es -> TupleLit (List.map (eval_expr items locals) es)
    | TupleProj (e, i) -> tuple_proj (eval_expr items locals e) i

  let rec quote_ty (vt : vty) : ty =
    match vt with
    | ByteTy -> ByteTy
    | RecordTy fs -> RecordTy (LabelMap.map quote_ty fs)
    | TupleTy ts -> TupleTy (List.map quote_ty ts)

  let rec quote_expr (ve : vexpr) : expr =
    match ve with
    | ByteLit c -> ByteLit c
    | RecordLit fs -> RecordLit (LabelMap.map quote_expr fs)
    | TupleLit ves -> TupleLit (List.map quote_expr ves)

  let normalise_expr (items : (string * item) list) (locals : local_env) (e : expr) : expr =
    quote_expr (eval_expr items locals e)


  (* Unification *)

  let rec unify_tys (vt1 : vty) (vt2 : vty) : bool =
    match vt1, vt2 with
    | ByteTy, ByteTy -> true
    | RecordTy fields1, RecordTy fields2 ->
      LabelMap.equal unify_tys fields1 fields2
    | TupleTy vts1, TupleTy vts2 ->
      List.equal unify_tys vts1 vts2
    | _, _ -> false

  (* Decode semantics *)

  exception DecodeFailure of int

  let get_byte input pos =
    if pos < Bytes.length input then
      Some (Bytes.unsafe_get input pos)
    else
      None

  let decode_format (p : program) (input : bytes) (pos : int) (f : format) : int * vexpr =
    let rec go (locals : local_env) input pos (f : format) : int * vexpr =
      match f.node with
      | Item name -> begin
          match List.assoc_opt name p.items with
          | Some (Format f) -> go locals input pos f
          | Some _ -> invalid_arg "not a format item"
          | None -> invalid_arg "unbound item variable"
      end
      | Fail _ -> raise (DecodeFailure pos)
      | Byte s -> begin
          match get_byte input pos with
          | Some c when ByteSet.mem c s -> pos + 1, ByteLit c
          | _ -> raise (DecodeFailure pos)
      end
      | Seq fs ->
          let pos, es = List.fold_left_map (go locals input) pos fs in
          pos, TupleLit es
      | Union (f0, f1) -> begin
          match get_byte input pos with
          | Some b when ByteSet.mem b f0.info.first -> go locals input pos f0
          | Some b when ByteSet.mem b f1.info.first -> go locals input pos f1
          | _ when f0.info.nullable -> go locals input pos f0
          | _ when f1.info.nullable -> go locals input pos f1
          | _ -> raise (DecodeFailure pos)
      end
      | Map (_, (_, e), f) ->
          let pos, ev = go locals input pos f in
          pos, eval_expr p.items (ev :: locals) e
    | Pure (_, e) -> pos, eval_expr p.items locals e
      | FlatMap (_, (_, f1), f0) ->
          let pos, ev0 = go locals input pos f0 in
          go (ev0 :: locals) input pos f1
    in
    go [] input pos f

end
