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


type ty =
  (* | Item of string *)
  | UnitTy
  | ByteTy
  | PairTy of ty * ty
  | RecordTy of (string * ty) list

type expr =
  (* | Item of string *)
  | Local of int
  | Ann of expr * ty
  | UnitLit
  | ByteLit of char
  | PairLit of expr * expr
  | PairFst of expr
  | PairSnd of expr
  | RecordLit of (string * expr) list
  | RecordProj of expr * string

type format_node =
  | Item of string
  | Fail of ty
  | Byte of ByteSet.t
  | Seq of format * format
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
  | RecordTy fields ->
      Format.fprintf ppf "{@ %a@ }"
        (Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
          (fun ppf (l, t) -> Format.fprintf ppf "%s@ :@ %a" l pp_print_ty t))
        fields
  | t -> Format.fprintf ppf "(%a)" pp_print_ty t

let rec pp_print_expr names ppf e =
  match e with
  | PairLit (e0, e1) ->
      Format.fprintf ppf "%a,@ %a"
        (pp_print_atomic_expr names) e0
        (pp_print_expr names) e1
  | e -> pp_print_atomic_expr names ppf e

and pp_print_atomic_expr names ppf e =
  match e with
  | Local index -> Format.pp_print_string ppf (List.nth names index)
  | UnitLit -> Format.fprintf ppf "()"
  | ByteLit c -> Format.fprintf ppf "%i" (Char.code c)
  | PairFst e -> Format.fprintf ppf "%a.1" (pp_print_atomic_expr names) e
  | PairSnd e -> Format.fprintf ppf "%a.2" (pp_print_atomic_expr names) e
  | RecordLit fields ->
      Format.fprintf ppf "{@ %a@ }"
        (Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
          (fun ppf (l, f) -> Format.fprintf ppf "%s@ :=@ %a" l (pp_print_expr names) f))
        fields
  | RecordProj (e, l) -> Format.fprintf ppf "%a.%s" (pp_print_atomic_expr names) e l
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
  | FlatMap (t, (n, f1), f0) ->
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
  | Byte s -> Format.fprintf ppf "@[%a@]" ByteSet.pp_print s (* TODO: Custom printing *)
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
    | UnitLit
    | ByteLit of char
    | PairLit of vexpr * vexpr
    | RecordLit of (string * vexpr) list

  type local_env =
    vexpr list


  (* Eliminators *)

  let pair_fst (ve : vexpr) : vexpr =
    match ve with
    | PairLit (e0, _) -> e0
    | _ -> invalid_arg "expected pair"

  let pair_snd (ve : vexpr) : vexpr =
    match ve with
    | PairLit (_, e1) -> e1
    | _ -> invalid_arg "expected pair"

  let record_proj (ve : vexpr) (l : string) : vexpr =
    match ve with
    | RecordLit fs -> List.assoc l fs
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
    | UnitLit -> UnitLit
    | ByteLit c -> ByteLit c
    | PairLit (e0, e1) -> PairLit (eval locals e0, eval locals e1)
    | PairFst e -> pair_fst (eval locals e)
    | PairSnd e -> pair_snd (eval locals e)
    | RecordLit fs -> RecordLit (List.map (fun (l, e) -> l, eval locals e) fs)
    | RecordProj (e, l) -> record_proj (eval locals e) l

  let rec quote (ve : vexpr) : expr =
    match ve with
    | UnitLit -> UnitLit
    | ByteLit c -> ByteLit c
    | PairLit (ve0, ve1) -> PairLit (quote ve0, quote ve1)
    | RecordLit fs -> RecordLit (List.map (fun (l, ve) -> l, quote ve) fs)

  let normalise (locals : local_env) (e : expr) : expr =
    quote (eval locals e)


  (* Decode semantics *)

  exception DecodeFailure of int

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
      | Fail _ -> raise (DecodeFailure pos)
      | Byte s -> begin
          match get_byte input pos with
          | Some c when ByteSet.mem c s -> pos + 1, ByteLit c
          | _ -> raise (DecodeFailure pos)
      end
      | Seq (f0, f1) ->
          let pos, e0 = go locals f0 input pos in
          let pos, e1 = go locals f1 input pos in
          pos, PairLit (e0, e1)
      | Union (f0, f1) -> begin
          match get_byte input pos with
          | Some b when ByteSet.mem b f0.info.first -> go locals f0 input pos
          | Some b when ByteSet.mem b f1.info.first -> go locals f1 input pos
          | _ when f0.info.nullable -> go locals f0 input pos
          | _ when f1.info.nullable -> go locals f1 input pos
          | _ -> raise (DecodeFailure pos)
      end
      | Map (_, (_, e), f) ->
          let pos, ev = go locals f input pos in
          pos, eval (ev :: locals) e
    | Pure (_, e) -> pos, eval locals e
      | FlatMap (_, (_, f1), f0) ->
          let pos, ev0 = go locals f0 input pos in
          go (ev0 :: locals) f1 input pos
    in
    go [] f input pos

end


module Refiner = struct

  (* Contexts *)

  type item =
    | Type of ty
    | Format of { repr : ty; info : FormatInfo.t }
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

    let item (name : item_var) : [`FormatExpected | `UnboundVariable] is_format_err =
      fun items _ ->
        match List.assoc_opt name items with
        | Some (Format { repr; info }) -> Ok { node = Item name; repr; info }
        | Some _ -> Error `FormatExpected
        | None -> Error `UnboundVariable

    let fail (t : is_ty) : is_format =
      fun items _ ->
        let t = t items in
        {
          node = Fail t;
          repr = t;
          info = FormatInfo.fail;
        }

    let byte (s : ByteSet.t) : is_format =
      fun _ _ ->
        {
          node = Byte s;
          repr = ByteTy;
          info = FormatInfo.byte s;
        }

    let seq (f0 : is_format) (f1 : is_format) : [`AmbiguousFormat] is_format_err =
      fun items locals ->
        let f0 = f0 items locals in
        let f1 = f1 items locals in
        if not (FormatInfo.separate f0.info f1.info) then
          Error `AmbiguousFormat
        else
          Ok {
            node = Seq (f0, f1);
            repr = PairTy (f0.repr, f1.repr);
            info = FormatInfo.seq f0.info f1.info;
          }

    let union (f0 : is_format) (f1 : is_format) : [`AmbiguousFormat | `ReprMismatch of ty * ty] is_format_err =
      fun items locals ->
        let f0 = f0 items locals in
        let f1 = f1 items locals in
        if not (FormatInfo.non_overlapping f0.info f1.info) then
          Error `AmbiguousFormat
        else if f0.repr <> f1.repr then
          Error (`ReprMismatch (f0.repr, f1.repr))
        else
          Ok {
            node = Union (f0, f1);
            repr = f0.repr;
            info = FormatInfo.union f0.info f1.info;
          }

    let pure (e : synth_ty) : is_format =
      fun items locals ->
        let e, t = e items locals in
        {
          node = Pure (t, e);
          repr = t;
          info = FormatInfo.empty;
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

    let flat_map (x, f1 : string * (local_var -> is_format)) (f0 : is_format) : [`AmbiguousFormat] is_format_err =
      fun items locals ->
        let f0 = f0 items locals in
        let f1 = f1 (List.length locals) items (f0.repr :: locals) in
        if not (FormatInfo.separate f0.info f1.info) then
          Error `AmbiguousFormat
        else
          Ok {
            node = FlatMap (f1.repr, (x, f1), f0);
            repr = f1.repr;
            info = FormatInfo.seq f0.info f1.info;
          }

    let repr (f : is_format) : is_ty =
      fun items ->
        (f items []).repr

  end

  module Structural = struct

    let item_ty (name : item_var) : [`TypeExpected | `UnboundVariable] is_ty_err =
      fun items ->
        match List.assoc_opt name items with
        (* | Some (Type t) -> Ok (Item name) *)
        | Some (Type t) -> Ok t
        | Some _ -> Error `TypeExpected
        | None -> Error `UnboundVariable

    let item_expr (name : item_var) : [`ExprExpected | `UnboundVariable] synth_ty_err =
      fun items _ ->
        match List.assoc_opt name items with
        (* | Some (Expr (e, t)) -> Ok (Item name, t) *)
        | Some (Expr (e, t)) -> Ok (e, t)
        | Some _ -> Error `ExprExpected
        | None -> Error `UnboundVariable

    let local (level : local_var) : [`UnboundVariable] synth_ty_err =
      fun _ locals ->
        let index = List.length locals - level - 1 in
        match List.nth_opt locals index with
        | Some t -> Ok (Local index, t)
        | None -> Error `UnboundVariable

    let conv (e : synth_ty) : [`TypeMismatch of ty * ty] check_ty_err =
      fun t items locals ->
        let e, t' = e items locals in
        if t = t' then Ok e else
          Error (`TypeMismatch (t, t'))

    let ann (e : check_ty) (t : is_ty) : synth_ty =
      fun items locals ->
        let t = t items in
        let e = e t items locals in
        Ann (e, t), t

  end

  module Unit = struct

    let form : is_ty =
      fun _ ->
        UnitTy

    let intro : synth_ty =
      fun _ _ ->
        UnitLit, UnitTy

  end

  module Byte = struct

    let form : is_ty =
      fun _ ->
        ByteTy

    let intro c : synth_ty =
      fun _ _ ->
        ByteLit c, ByteTy

  end

  module Pair = struct

    let form (t0 : is_ty) (t1 : is_ty) : is_ty =
      fun items ->
        let t0 = t0 items in
        let t1 = t1 items in
        PairTy (t0, t1)

    let intro (e0 : synth_ty) (e1 : synth_ty) : synth_ty =
      fun items locals ->
        let e0, t0 = e0 items locals in
        let e1, t1 = e1 items locals in
        PairLit (e0, e1), PairTy (t0, t1)

    let fst (e : synth_ty) : [`UnexpectedType] synth_ty_err =
      fun items locals ->
        let e, t = e items locals in
        match t with
        | PairTy (t0, _) -> Ok (PairFst e, t0)
        | _ -> Error `UnexpectedType

    let snd (e : synth_ty) : [`UnexpectedType] synth_ty_err =
      fun items locals ->
        let e, t = e items locals in
        match t with
        | PairTy (_, t1) -> Ok (PairSnd e, t1)
        | _ -> Error `UnexpectedType

  end

  module Record = struct

    let ( let* ) = Result.bind

    let form_empty : is_ty =
      fun _ ->
        RecordTy []

    let form (fields : (string * is_ty) list) : [`DuplicateFieldLabel of string] is_ty_err =
      fun items ->
        let rec go seen =
          function
          | [] -> Ok []
          | (l, _) :: _ when List.mem l seen ->
              Error (`DuplicateFieldLabel l) (* TODO: Collect multiple dupes *)
          | (l, t) :: fields ->
              let t = t items in
              let* t_fields = go (l :: seen) fields in (* TODO: Make tail recursive *)
              Ok ((l, t) :: t_fields)
        in
        let* t_fields = go [] fields in
        Ok (RecordTy t_fields)

    let intro_empty : synth_ty =
      fun _ _ ->
        RecordLit [], RecordTy []

    let intro (fields : (string * synth_ty) list) : [`DuplicateFieldLabel of string] synth_ty_err =
      fun items locals ->
        let rec go seen =
          function
          | [] -> Ok ([], [])
          | (l, _) :: _ when List.mem l seen ->
              Error (`DuplicateFieldLabel l) (* TODO: Collect multiple dupes *)
          | (l, t) :: fields ->
              let e, t = t items locals in
              let* e_fields, t_fields = go (l :: seen) fields in (* TODO: Make tail recursive *)
              Ok ((l, e) :: e_fields, (l, t) :: t_fields)
        in
        let* e_fields, t_fields = go [] fields in
        Ok (RecordLit e_fields, RecordTy t_fields)

    let proj (e : synth_ty) (l : string) : [`UnknownFieldLabel of ty] synth_ty_err =
      fun items locals ->
        let e, t = e items locals in
        match t with
        | RecordTy fs -> begin
            match List.assoc_opt l fs with
            | Some t -> Ok (RecordProj (e, l), t)
            | None -> Error (`UnknownFieldLabel t)
        end
        | _ -> Error (`UnknownFieldLabel t)

  end

end
