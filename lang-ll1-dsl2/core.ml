module Format_info : sig
  (** Semantic information about a binary format. *)

  type t = {
    nullable : bool;
    (** The {i nullability predicate}, i.e. whether the parser might succeed
        while consuming no input. *)

    first : Byte_set.t;
    (** The {i first set}, i.e. the set of bytes that can appear as the first
        byte of this parser. *)

    follow_last : Byte_set.t;
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

  val byte : Byte_set.t -> t
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
    first : Byte_set.t;
    follow_last : Byte_set.t;
  }

  let empty = {
    nullable = true; (* Never consumes any input *)
    first = Byte_set.empty;
    follow_last = Byte_set.empty;
  }

  let byte s = {
    nullable = false; (* Always consumes exactly one byte from the input *)
    first = s;
    follow_last = Byte_set.empty;
  }

  let fail = byte Byte_set.empty

  let separate i1 i2 =
    (* TODO: Could it be ok for either [i1] or [i2] to be nullable? *)
    not i1.nullable && Byte_set.disjoint i1.follow_last i2.first

  let non_overlapping i1 i2 =
    not (i1.nullable && i2.nullable) && Byte_set.disjoint i1.first i2.first

  let seq i1 i2 = {
    nullable = i1.nullable && i2.nullable;
    first = i1.first;
    follow_last =
      Byte_set.union
        i2.follow_last
        (if i2.nullable
          then Byte_set.union i2.first i1.follow_last
          else Byte_set.empty);
  }

  let union i1 i2 = {
    nullable = i1.nullable || i2.nullable;
    first = Byte_set.union i1.first i2.first;
    follow_last = Byte_set.union i1.follow_last i2.follow_last;
  }

end

(** An unordered row of elements distinguished by label. *)
module Label_map = Map.Make (String)

[@@@warning "-duplicate-definitions"]

type ty =
  | Item of string
  | Byte_ty
  | Record_ty of ty Label_map.t
  | Tuple_ty of ty list
  | Format_repr of format

and expr =
  | Item of string
  | Local of int
  | Byte_lit of char
  | Record_lit of expr Label_map.t
  | Record_proj of expr * string
  | Tuple_lit of expr list
  | Tuple_proj of expr * int

and format_node =
  | Item of string
  | Fail of ty
  | Byte of Byte_set.t
  | Seq of format list
  | Union of format * format
  | Pure of ty * expr
  | Map of ty * (string * expr) * format
  | Flat_map of ty * (string * format) * format

and format = {
  node : format_node;
  info : Format_info.t;
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
  | Byte_ty -> Format.fprintf ppf "Byte"
  | Record_ty fields ->
      Format.fprintf ppf "{@ %a@ }"
        (Format.pp_print_seq
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
          (fun ppf (l, t) -> Format.fprintf ppf "%s@ :@ %a" l pp_print_ty t))
        (Label_map.to_seq fields)
  | Tuple_ty ts ->
      Format.fprintf ppf "(%a)"
        (Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
          pp_print_ty)
        ts
  | Format_repr f ->
      Format.fprintf ppf "%a.Repr"
        (pp_print_atomic_format []) f
  (* | t -> Format.fprintf ppf "(%a)" pp_print_ty t *)

and pp_print_expr names ppf (e : expr) =
  match e with
  | e -> pp_proj_expr names ppf e

and pp_proj_expr names ppf e =
  match e with
  | Record_proj (e, l) -> Format.fprintf ppf "%a.%s" (pp_proj_expr names) e l
  | Tuple_proj (e, i) -> Format.fprintf ppf "%a.%i" (pp_proj_expr names) e i
  | e -> pp_print_atomic_expr names ppf e

and pp_print_atomic_expr names ppf e =
  match e with
  | Item name -> Format.pp_print_string ppf name
  | Local index -> Format.pp_print_string ppf (List.nth names index)
  | Byte_lit c -> Format.fprintf ppf "%i" (Char.code c)
  | Record_lit fields ->
      Format.fprintf ppf "{@ %a@ }"
        (Format.pp_print_seq
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
          (fun ppf (l, f) -> Format.fprintf ppf "%s@ :=@ %a" l (pp_print_expr names) f))
        (Label_map.to_seq fields)
  | Tuple_lit es ->
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
  | Flat_map (t, (n, f1), f0) ->
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
  | Byte s -> Format.fprintf ppf "@[%a@]" Byte_set.pp_print s (* TODO: Custom printing *)
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
    | Byte_ty
    | Record_ty of vty Label_map.t
    | Tuple_ty of vty list

  type vexpr =
    | Byte_lit of char
    | Record_lit of vexpr Label_map.t
    | Tuple_lit of vexpr list

  type local_env =
    vexpr list


  (* Eliminators *)

  let record_proj (ve : vexpr) (l : string) : vexpr =
    match ve with
    | Record_lit fs -> Label_map.find l fs
    | _ -> invalid_arg "expected pair"

  let tuple_proj (ve : vexpr) (i : int) : vexpr =
    match ve with
    | Tuple_lit ves -> List.nth ves i
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
    | Byte_ty -> Byte_ty
    | Record_ty fs -> Record_ty (Label_map.map (eval_ty items) fs)
    | Tuple_ty ts -> Tuple_ty (List.map (eval_ty items) ts)
    | Format_repr f -> eval_format_repr items f

  and eval_format_repr (items : (string * item) list) (f : format) : vty =
    match f.node with
    | Item n -> begin
      match List.assoc_opt n items with
      | Some (Format f) -> eval_format_repr items f
      | Some _ -> invalid_arg "expected format"
      | None -> invalid_arg "unbound item variable"
    end
    | Fail t -> eval_ty items t
    | Byte _ -> Byte_ty
    | Seq fs -> Tuple_ty (List.map (eval_format_repr items) fs)
    | Union (f1, _) -> eval_format_repr items f1
    | Map (t, (_, _), _) -> eval_ty items t
    | Pure (t, _) -> eval_ty items t
    | Flat_map (t, (_, _), _) -> eval_ty items t

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
    | Byte_lit c -> Byte_lit c
    | Record_lit fs -> Record_lit (Label_map.map (eval_expr items locals) fs)
    | Record_proj (e, l) -> record_proj (eval_expr items locals e) l
    | Tuple_lit es -> Tuple_lit (List.map (eval_expr items locals) es)
    | Tuple_proj (e, i) -> tuple_proj (eval_expr items locals e) i

  let rec quote_ty (vt : vty) : ty =
    match vt with
    | Byte_ty -> Byte_ty
    | Record_ty fs -> Record_ty (Label_map.map quote_ty fs)
    | Tuple_ty ts -> Tuple_ty (List.map quote_ty ts)

  let rec quote_expr (ve : vexpr) : expr =
    match ve with
    | Byte_lit c -> Byte_lit c
    | Record_lit fs -> Record_lit (Label_map.map quote_expr fs)
    | Tuple_lit ves -> Tuple_lit (List.map quote_expr ves)

  let normalise_expr (items : (string * item) list) (locals : local_env) (e : expr) : expr =
    quote_expr (eval_expr items locals e)


  (* Unification *)

  let rec unify_tys (vt1 : vty) (vt2 : vty) : bool =
    match vt1, vt2 with
    | Byte_ty, Byte_ty -> true
    | Record_ty fields1, Record_ty fields2 ->
      Label_map.equal unify_tys fields1 fields2
    | Tuple_ty vts1, Tuple_ty vts2 ->
      List.equal unify_tys vts1 vts2
    | _, _ -> false

  (* Decode semantics *)

  exception Decode_failure of int

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
      | Fail _ -> raise (Decode_failure pos)
      | Byte s -> begin
          match get_byte input pos with
          | Some c when Byte_set.mem c s -> pos + 1, Byte_lit c
          | _ -> raise (Decode_failure pos)
      end
      | Seq fs ->
          let pos, es = List.fold_left_map (go locals input) pos fs in
          pos, Tuple_lit es
      | Union (f0, f1) -> begin
          match get_byte input pos with
          | Some b when Byte_set.mem b f0.info.first -> go locals input pos f0
          | Some b when Byte_set.mem b f1.info.first -> go locals input pos f1
          | _ when f0.info.nullable -> go locals input pos f0
          | _ when f1.info.nullable -> go locals input pos f1
          | _ -> raise (Decode_failure pos)
      end
      | Map (_, (_, e), f) ->
          let pos, ev = go locals input pos f in
          pos, eval_expr p.items (ev :: locals) e
      | Pure (_, e) -> pos, eval_expr p.items locals e
      | Flat_map (_, (_, f1), f0) ->
          let pos, ev0 = go locals input pos f0 in
          go (ev0 :: locals) input pos f1
    in
    go [] input pos f

end
