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


type ty =
  (* | Item of string *)
  | Unit_ty
  | Byte_ty
  | Pair_ty of ty * ty
  | Record_ty of (string * ty) list

type expr =
  (* | Item of string *)
  | Local of int
  | Ann of expr * ty
  | Unit_lit
  | Byte_lit of char
  | Pair_lit of expr * expr
  | Pair_fst of expr
  | Pair_snd of expr
  | Record_lit of (string * expr) list
  | Record_proj of expr * string

type format_node =
  | Item of string
  | Fail of ty
  | Byte of Byte_set.t
  | Seq of format * format
  | Union of format * format
  | Pure of ty * expr
  | Map of ty * (string * expr) * format
  | Flat_map of ty * (string * format) * format
and format = {
  node : format_node;
  repr : ty;
  info : Format_info.t;
}

type item =
  | Type of ty
  | Format of format
  | Expr of expr * ty

type program = {
  items : (string * item) list;
}

(* Unification *)

let rec unify_ty (t1 : ty) (t2 : ty) : bool =
  match t1, t2 with
  | Unit_ty, Unit_ty -> true
  | Byte_ty, Byte_ty -> true
  | Pair_ty (t1_1, t2_1), Pair_ty (t1_2, t2_2) ->
      unify_ty t1_1 t1_2 && unify_ty t2_1 t2_2
  | Record_ty fields1, Record_ty fields2 ->
      (* There might be a nicer way to do this... *)
      let fields1 = List.sort (fun (l1, _) (l2, _) -> String.compare l1 l2) fields1 in
      let fields2 = List.sort (fun (l1, _) (l2, _) -> String.compare l1 l2) fields2 in
      List.equal (fun (l1, t1) (l2, t2) -> l1 = l2 && unify_ty t1 t2) fields1 fields2
  | _, _ -> false


(* Pretty printing *)

let rec pp_print_ty ppf t =
  match t with
  | Pair_ty (t0, t1) ->
      Format.fprintf ppf "Pair@ %a@ %a"
        pp_print_atomic_ty t0
        pp_print_atomic_ty t1
  | t -> pp_print_atomic_ty ppf t

and pp_print_atomic_ty ppf t =
  match t with
  | Unit_ty -> Format.fprintf ppf "Unit"
  | Byte_ty -> Format.fprintf ppf "Byte"
  | Record_ty fields ->
      Format.fprintf ppf "{@ %a@ }"
        (Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
          (fun ppf (l, t) -> Format.fprintf ppf "%s@ :@ %a" l pp_print_ty t))
        fields
  | t -> Format.fprintf ppf "(%a)" pp_print_ty t

let rec pp_print_expr names ppf e =
  match e with
  | Pair_lit (e0, e1) ->
      Format.fprintf ppf "%a,@ %a"
        (pp_print_atomic_expr names) e0
        (pp_print_expr names) e1
  | e -> pp_print_atomic_expr names ppf e

and pp_print_atomic_expr names ppf e =
  match e with
  | Local index -> Format.pp_print_string ppf (List.nth names index)
  | Unit_lit -> Format.fprintf ppf "()"
  | Byte_lit c -> Format.fprintf ppf "%i" (Char.code c)
  | Pair_fst e -> Format.fprintf ppf "%a.1" (pp_print_atomic_expr names) e
  | Pair_snd e -> Format.fprintf ppf "%a.2" (pp_print_atomic_expr names) e
  | Record_lit fields ->
      Format.fprintf ppf "{@ %a@ }"
        (Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
          (fun ppf (l, f) -> Format.fprintf ppf "%s@ :=@ %a" l (pp_print_expr names) f))
        fields
  | Record_proj (e, l) -> Format.fprintf ppf "%a.%s" (pp_print_atomic_expr names) e l
  | e -> Format.fprintf ppf "(%a)" (pp_print_atomic_expr names) e

let rec pp_print_format names ppf f =
  pp_print_union_format names ppf f

and pp_print_union_format names ppf f =
  match f.node with
  | Union (f0, f1) ->
      Format.fprintf ppf "@[%a@]@ |@ %a"
        (pp_print_seq_format names) f0
        (pp_print_union_format names) f1
  | _ ->
      pp_print_seq_format names ppf f

and pp_print_seq_format names ppf f =
  match f.node with
  | Seq (f0, f1) ->
      Format.fprintf ppf "@[%a,@]@ %a"
        (pp_print_app_format names) f0
        (pp_print_seq_format names) f1
  | _ ->
      pp_print_app_format names ppf f

and pp_print_app_format names ppf f =
  match f.node with
  | Fail t ->
      Format.fprintf ppf "@[fail@ @@%a@]"
        pp_print_atomic_ty t
  | Pure (t, e) ->
      Format.fprintf ppf "@[pure@ @@%a@ %a@]"
        pp_print_atomic_ty t
        (pp_print_expr names) e
  | Map (t, (n, e), f) ->
      Format.fprintf ppf "@[map@ @@%a@ (%s@ =>@ %a)@ %a@]"
        pp_print_atomic_ty t
        n
        (pp_print_expr (n :: names)) e
        (pp_print_atomic_format names) f
  | Flat_map (t, (n, f1), f0) ->
      Format.fprintf ppf "@[flat-map@ @@%a@ (%s@ =>@ %a)@ %a@]"
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

  type vexpr =
    | Unit_lit
    | Byte_lit of char
    | Pair_lit of vexpr * vexpr
    | Record_lit of (string * vexpr) list

  type local_env =
    vexpr list


  (* Eliminators *)

  let pair_fst (ve : vexpr) : vexpr =
    match ve with
    | Pair_lit (e0, _) -> e0
    | _ -> invalid_arg "expected pair"

  let pair_snd (ve : vexpr) : vexpr =
    match ve with
    | Pair_lit (_, e1) -> e1
    | _ -> invalid_arg "expected pair"

  let record_proj (ve : vexpr) (l : string) : vexpr =
    match ve with
    | Record_lit fs -> List.assoc l fs
    | _ -> invalid_arg "expected pair"

  (* Evalution *)

  let rec eval (locals : local_env) (e : expr) : vexpr =
    match e with
    | Local x -> begin
        match List.nth_opt locals x with
        | Some v -> v
        | None -> invalid_arg "unbound local variable"
    end
    | Ann (e, _) -> eval locals e
    | Unit_lit -> Unit_lit
    | Byte_lit c -> Byte_lit c
    | Pair_lit (e0, e1) -> Pair_lit (eval locals e0, eval locals e1)
    | Pair_fst e -> pair_fst (eval locals e)
    | Pair_snd e -> pair_snd (eval locals e)
    | Record_lit fs -> Record_lit (List.map (fun (l, e) -> l, eval locals e) fs)
    | Record_proj (e, l) -> record_proj (eval locals e) l

  let rec quote (ve : vexpr) : expr =
    match ve with
    | Unit_lit -> Unit_lit
    | Byte_lit c -> Byte_lit c
    | Pair_lit (ve0, ve1) -> Pair_lit (quote ve0, quote ve1)
    | Record_lit fs -> Record_lit (List.map (fun (l, ve) -> l, quote ve) fs)

  let normalise (locals : local_env) (e : expr) : expr =
    quote (eval locals e)


  (* Decode semantics *)

  exception Decode_failure of int

  let get_byte input pos =
    if pos < Bytes.length input then
      Some (Bytes.unsafe_get input pos)
    else
      None

  let decode (p : program) (f : format) input pos : int * vexpr =
    let rec go (locals : local_env) (f : format) input pos : int * vexpr =
      match f.node with
      | Item name -> begin
          match List.assoc_opt name p.items with
          | Some (Format f) -> go locals f input pos
          | Some _ -> invalid_arg "not a format item"
          | None -> invalid_arg "unbound item variable"
      end
      | Fail _ -> raise (Decode_failure pos)
      | Byte s -> begin
          match get_byte input pos with
          | Some c when Byte_set.mem c s -> pos + 1, Byte_lit c
          | _ -> raise (Decode_failure pos)
      end
      | Seq (f0, f1) ->
          let pos, e0 = go locals f0 input pos in
          let pos, e1 = go locals f1 input pos in
          pos, Pair_lit (e0, e1)
      | Union (f0, f1) -> begin
          match get_byte input pos with
          | Some b when Byte_set.mem b f0.info.first -> go locals f0 input pos
          | Some b when Byte_set.mem b f1.info.first -> go locals f1 input pos
          | _ when f0.info.nullable -> go locals f0 input pos
          | _ when f1.info.nullable -> go locals f1 input pos
          | _ -> raise (Decode_failure pos)
      end
      | Map (_, (_, e), f) ->
          let pos, ev = go locals f input pos in
          pos, eval (ev :: locals) e
    | Pure (_, e) -> pos, eval locals e
      | Flat_map (_, (_, f1), f0) ->
          let pos, ev0 = go locals f0 input pos in
          go (ev0 :: locals) f1 input pos
    in
    go [] f input pos

end


module Refiner = struct

  (* Contexts *)

  type item =
    | Type of ty
    | Format of { repr : ty; info : Format_info.t }
    | Expr of expr * ty

  type item_context = (string * item) list
  type local_context = ty list


  (* Context effects *)

  type 'a item_m = item_context -> 'a
  type 'a local_m = item_context -> local_context -> 'a

  type ('a, 'e) item_err_m = ('a, 'e) result item_m
  type ('a, 'e) local_err_m = ('a, 'e) result local_m

  let handle_item (handler : 'e -> 'a item_m) (m : ('a, 'e) item_err_m) : 'a item_m =
    fun items ->
      match m items with
      | Ok x -> x
      | Error e -> (handler e) items

  let handle_local (handler : 'e -> 'a local_m) (m : ('a, 'e) local_err_m) : 'a local_m =
    fun items locals ->
      match m items locals with
      | Ok x -> x
      | Error e -> (handler e) items locals

  let run_item (m : 'a item_m) : 'a =
    m []

  let run_local (m : 'a local_m) : 'a =
    m [] []


  (* Forms of judgement *)

  type local_var = int
  type item_var = string

  type is_program = program item_m
  type is_format = format local_m
  type is_ty = ty item_m
  type synth_ty = (expr * ty) local_m
  type check_ty = ty -> expr local_m

  type 'e is_program_err = (program, 'e) item_err_m
  type 'e is_format_err = (format, 'e) local_err_m
  type 'e is_ty_err = (ty, 'e) item_err_m
  type 'e synth_ty_err = (expr * ty, 'e) local_err_m
  type 'e check_ty_err = ty -> (expr, 'e) local_err_m


  (* Inference rules *)

  module Program = struct

    let empty : is_program =
      fun _ ->
        { items = [] }

    let def_ty (name, t) (body : item_var -> is_program) : is_program =
      fun items ->
        let t = t items in
        let program = body name ((name, Type t) :: items) in
        { items = (name, Type t) :: program.items }

    let def_format (name, f) (body : item_var -> is_program) : is_program =
      fun items ->
        let f = f items [] in
        let program = body name ((name, Format { repr = f.repr; info = f.info }) :: items) in
        { items = (name, Format f) :: program.items }

    let def_expr (name, t, e) (body : item_var -> is_program) : is_program =
      fun items ->
        let t = t items in
        let e = e t items [] in
        let program = body name ((name, Expr (e, t)) :: items) in
        { items = (name, Expr (e, t)) :: program.items }

  end

  module Format = struct

    let item (name : item_var) : [`Format_expected | `Unbound_variable] is_format_err =
      fun items _ ->
        match List.assoc_opt name items with
        | Some (Format { repr; info }) -> Ok { node = Item name; repr; info }
        | Some _ -> Error `Format_expected
        | None -> Error `Unbound_variable

    let fail (t : is_ty) : is_format =
      fun items _ ->
        let t = t items in
        {
          node = Fail t;
          repr = t;
          info = Format_info.fail;
        }

    let byte (s : Byte_set.t) : is_format =
      fun _ _ ->
        {
          node = Byte s;
          repr = Byte_ty;
          info = Format_info.byte s;
        }

    let seq (f0 : is_format) (f1 : is_format) : [`Ambiguous_format] is_format_err =
      fun items locals ->
        let f0 = f0 items locals in
        let f1 = f1 items locals in
        if not (Format_info.separate f0.info f1.info) then
          Error `Ambiguous_format
        else
          Ok {
            node = Seq (f0, f1);
            repr = Pair_ty (f0.repr, f1.repr);
            info = Format_info.seq f0.info f1.info;
          }

    let union (f0 : is_format) (f1 : is_format) : [`Ambiguous_format | `Repr_mismatch of ty * ty] is_format_err =
      fun items locals ->
        let f0 = f0 items locals in
        let f1 = f1 items locals in
        if not (Format_info.non_overlapping f0.info f1.info) then
          Error `Ambiguous_format
        else if not (unify_ty f0.repr f1.repr) then
          Error (`Repr_mismatch (f0.repr, f1.repr))
        else
          Ok {
            node = Union (f0, f1);
            repr = f0.repr;
            info = Format_info.union f0.info f1.info;
          }

    let pure (e : synth_ty) : is_format =
      fun items locals ->
        let e, t = e items locals in
        {
          node = Pure (t, e);
          repr = t;
          info = Format_info.empty;
        }

    let map (x, e : string * (local_var -> synth_ty)) (f : is_format) : is_format =
      fun items locals ->
        let f = f items locals in
        let e, t = e (List.length locals) items (f.repr :: locals) in
        {
          node = Map (t, (x, e), f);
          repr = t;
          info = f.info;
        }

    let flat_map (x, f1 : string * (local_var -> is_format)) (f0 : is_format) : [`Ambiguous_format] is_format_err =
      fun items locals ->
        let f0 = f0 items locals in
        let f1 = f1 (List.length locals) items (f0.repr :: locals) in
        if not (Format_info.separate f0.info f1.info) then
          Error `Ambiguous_format
        else
          Ok {
            node = Flat_map (f1.repr, (x, f1), f0);
            repr = f1.repr;
            info = Format_info.seq f0.info f1.info;
          }

    let repr (f : is_format) : is_ty =
      fun items ->
        (f items []).repr

  end

  module Structural = struct

    let item_ty (name : item_var) : [`Type_expected | `Unbound_variable] is_ty_err =
      fun items ->
        match List.assoc_opt name items with
        (* | Some (Type t) -> Ok (Item name) *)
        | Some (Type t) -> Ok t
        | Some _ -> Error `Type_expected
        | None -> Error `Unbound_variable

    let item_expr (name : item_var) : [`Expr_expected | `Unbound_variable] synth_ty_err =
      fun items _ ->
        match List.assoc_opt name items with
        (* | Some (Expr (e, t)) -> Ok (Item name, t) *)
        | Some (Expr (e, t)) -> Ok (e, t)
        | Some _ -> Error `Expr_expected
        | None -> Error `Unbound_variable

    let local (level : local_var) : [`Unbound_variable] synth_ty_err =
      fun _ locals ->
        let index = List.length locals - level - 1 in
        match List.nth_opt locals index with
        | Some t -> Ok (Local index, t)
        | None -> Error `Unbound_variable

    let conv (e : synth_ty) : [`Type_mismatch of ty * ty] check_ty_err =
      fun t items locals ->
        let e, t' = e items locals in
        if unify_ty t t' then Ok e else
          Error (`Type_mismatch (t, t'))

    let ann (e : check_ty) (t : is_ty) : synth_ty =
      fun items locals ->
        let t = t items in
        let e = e t items locals in
        Ann (e, t), t

  end

  module Unit = struct

    let form : is_ty =
      fun _ ->
        Unit_ty

    let intro : synth_ty =
      fun _ _ ->
        Unit_lit, Unit_ty

  end

  module Byte = struct

    let form : is_ty =
      fun _ ->
        Byte_ty

    let intro c : synth_ty =
      fun _ _ ->
        Byte_lit c, Byte_ty

  end

  module Pair = struct

    let form (t0 : is_ty) (t1 : is_ty) : is_ty =
      fun items ->
        let t0 = t0 items in
        let t1 = t1 items in
        Pair_ty (t0, t1)

    let intro (e0 : synth_ty) (e1 : synth_ty) : synth_ty =
      fun items locals ->
        let e0, t0 = e0 items locals in
        let e1, t1 = e1 items locals in
        Pair_lit (e0, e1), Pair_ty (t0, t1)

    let fst (e : synth_ty) : [`Unexpected_type] synth_ty_err =
      fun items locals ->
        let e, t = e items locals in
        match t with
        | Pair_ty (t0, _) -> Ok (Pair_fst e, t0)
        | _ -> Error `Unexpected_type

    let snd (e : synth_ty) : [`Unexpected_type] synth_ty_err =
      fun items locals ->
        let e, t = e items locals in
        match t with
        | Pair_ty (_, t1) -> Ok (Pair_snd e, t1)
        | _ -> Error `Unexpected_type

  end

  module Record = struct

    let ( let* ) = Result.bind

    let form_empty : is_ty =
      fun _ ->
        Record_ty []

    let form (fields : (string * is_ty) list) : [`Duplicate_field_label of string] is_ty_err =
      fun items ->
        let rec go seen =
          function
          | [] -> Ok []
          | (l, _) :: _ when List.mem l seen ->
              Error (`Duplicate_field_label l) (* TODO: Collect multiple dupes *)
          | (l, t) :: fields ->
              let t = t items in
              let* t_fields = go (l :: seen) fields in (* TODO: Make tail recursive *)
              Ok ((l, t) :: t_fields)
        in
        let* t_fields = go [] fields in
        Ok (Record_ty t_fields)

    let intro_empty : synth_ty =
      fun _ _ ->
        Record_lit [], Record_ty []

    let intro (fields : (string * synth_ty) list) : [`Duplicate_field_label of string] synth_ty_err =
      fun items locals ->
        let rec go seen =
          function
          | [] -> Ok ([], [])
          | (l, _) :: _ when List.mem l seen ->
              Error (`Duplicate_field_label l) (* TODO: Collect multiple dupes *)
          | (l, t) :: fields ->
              let e, t = t items locals in
              let* e_fields, t_fields = go (l :: seen) fields in (* TODO: Make tail recursive *)
              Ok ((l, e) :: e_fields, (l, t) :: t_fields)
        in
        let* e_fields, t_fields = go [] fields in
        Ok (Record_lit e_fields, Record_ty t_fields)

    let proj (e : synth_ty) (l : string) : [`Unknown_field_label of ty] synth_ty_err =
      fun items locals ->
        let e, t = e items locals in
        match t with
        | Record_ty fs -> begin
            match List.assoc_opt l fs with
            | Some t -> Ok (Record_proj (e, l), t)
            | None -> Error (`Unknown_field_label t)
        end
        | _ -> Error (`Unknown_field_label t)

  end

end
