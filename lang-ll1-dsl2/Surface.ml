(** {0 Surface language}

    The surface language closely mirrors what the programmer originaly wrote,
    including syntactic sugar and higher level language features that make
    programming more convenient (in comparison to the {!Core}).
*)

(** {1 Syntax} *)

(** The start and end position in a source file *)
type loc =
  Lexing.position * Lexing.position

(** Located nodes *)
type 'a located = {
  loc : loc;
  data : 'a;
}

(** Names that bind definitions or parameters *)
type binder = string located

type tm =
  tm_node located

and tm_node =
  | Name of string
  | Ann of tm * tm
  | RecordEmpty
  | RecordTy of (string located * tm) list
  | RecordLit of (string located * tm) list
  | RecordFormat of (string located * tm) list
  | Tuple of tm list
  | IntLit of int
  | Proj of tm * [`Label of string located | `Index of int located]
  | Action of tm * (binder * tm)
  | Union of tm * tm
  | Not of tm
  | Range of bound * bound

and bound =
  | Open
  | Inclusive of int located
  | Exclusive of int located

type item =
  | FormatDef of binder * tm
  | TypeDef of binder * tm
  | Def of binder * tm option * tm

type program =
  item list


(** {1 Elaboration} *)

module Elab = struct

  module LabelMap = Core.LabelMap

  exception Error of loc * string
  exception Bug of loc * string

  let error loc msg = raise (Error (loc, msg))
  let bug loc msg = raise (Bug (loc, msg))

  (** An inferred term *)
  type infer_tm =
    | InferKind of [`Type | `Format]
    | InferType of Core.ty
    | InferExpr of Core.expr * Core.ty
    | InferFormat of Core.format


  type context = {
    items : (string * Core.item) list;
    locals : (string * Core.ty) list;
  }

  let lookup_local (ctx : context) (n : string) : (Core.expr * Core.ty) option =
    ctx.locals |> List.find_mapi @@ fun i (n', t) ->
      if n = n' then Some (Core.Local i, t) else None


  (** Byte conversions *)

  let byte_of_int loc i =
    if 0 <= i && i <= 255 then
      Char.chr i
    else
      error loc (Format.asprintf "integer `%i` is outside the range `0..255`" i)

  let byte_set_of_int loc i =
    ByteSet.singleton (byte_of_int loc i)

  let byte_set_of_range (start : bound) (stop : bound) =
    let start =
      match start with
      | Open -> Char.chr 0
      | Inclusive start -> byte_of_int start.loc start.data
      | Exclusive start -> byte_of_int start.loc (start.data + 1)
    and stop =
      match stop with
      | Open -> Char.chr 255
      | Inclusive stop -> byte_of_int stop.loc stop.data
      | Exclusive stop -> byte_of_int stop.loc (stop.data - 1)
    in
    ByteSet.range start stop

  let format_of_byte_set s : Core.format = {
    node = Byte s;
    repr = ByteTy;
    info = Core.FormatInfo.byte s;
  }


  (** {2 Bidirectional type checking} *)

  let equate_ty (loc : loc) (ty1 : Core.ty) (ty2 : Core.ty) =
    if ty1 = ty2 then () else
      error loc
        (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %a@]@ @[found: %a@]@]"
          Core.pp_print_ty ty1
          Core.pp_print_ty ty2)

  (** Elaborate a surface term into a core type. *)
  let rec check_type (ctx : context) (tm : tm) : Core.ty =
    match tm.data with
    (* Empty records *)
    | RecordEmpty ->
      RecordTy LabelMap.empty

    (* Tuples *)
    | Tuple tms ->
      TupleTy (List.map (check_type ctx) tms)

    (* Integer literals *)
    | IntLit _ ->
      error tm.loc "unexpected integer literal"

    (* Conversion *)
    | _ ->
      match infer ctx tm with
      | InferKind _ -> error tm.loc "expected type, found kind"
      | InferType t -> t
      | InferExpr (_, _) -> error tm.loc "expected type, found expression"
      | InferFormat f -> f.repr
      (*                 ^^^^^^ TODO: preserve `repr` in core language *)

  (** Elaborate a surface term into a core expression, given an expected type. *)
  and check_expr (ctx : context) (tm : tm) (t : Core.ty) : Core.expr =
    match tm.data, t with
    (* Empty records *)

    | RecordEmpty, t ->
      equate_ty tm.loc t (RecordTy LabelMap.empty);
      RecordLit LabelMap.empty

    (* Tuples *)

    | Tuple tms, TupleTy ts ->
      let rec go tms ts =
        match tms, ts with
        | [], [] -> []
        | tm :: tms, t :: ts -> check_expr ctx tm t :: go tms ts
        | _, _ -> error tm.loc "unexpected number of elements in tuple"
      in
      TupleLit (go tms ts)

    | Tuple _, t ->
      error tm.loc
        (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %a@]@ @[found: tuple@]@]"
          Core.pp_print_ty t)

    (* Integer literals *)

    | IntLit i, ByteTy -> ByteLit (byte_of_int tm.loc i)
    | IntLit _, _ -> error tm.loc "unexpected integer literal"

    (* Conversion *)

    | _, t ->
      let e', t' = infer_expr ctx tm in
      equate_ty tm.loc t t';
      e'

  (** Elaborate a surface term into a core format. *)
  and check_format (ctx : context) (tm : tm) : Core.format =
    match tm.data with
    (* Empty records *)
    | RecordEmpty ->
      Core.{
        node = Pure (RecordTy LabelMap.empty, RecordLit LabelMap.empty);
        repr = RecordTy LabelMap.empty;
        info = FormatInfo.empty;
      }

    (* Tuples *)
    | Tuple tms ->
      let fs = List.map (check_format ctx) tms in
      Core.{
        node = Seq fs;
        repr = TupleTy (List.map (fun f -> f.repr) fs);
        info = List.fold_right (fun f acc -> FormatInfo.seq f.info acc) fs FormatInfo.empty;
      }

    (* Integer literals *)
    | IntLit i ->
      format_of_byte_set (byte_set_of_int tm.loc i)

    (* Conversion *)
    | _ ->
      infer_format ctx tm

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer (ctx : context) (tm : tm) : infer_tm =
    match tm.data with
    | Name n -> begin
      match lookup_local ctx n with
      | Some (e, t) -> InferExpr (e, t)
      | None -> begin
        match List.assoc_opt n ctx.items with
        | Some (Type _) -> InferType (Item n)
        | Some (Format f) -> InferFormat { f with node = Item n }
        | Some (Expr (_, ty)) -> InferExpr (Item n, ty)
        | None when n = "Type" -> InferKind `Type
        | None when n = "Format" -> InferKind `Format
        | None when n = "Byte" -> InferType ByteTy
        | None -> error tm.loc (Format.asprintf "unbound name `%s`" n)
      end
    end

    | Ann (tm, ann) -> begin
      match infer ctx ann with
      | InferKind `Type -> InferType (check_type ctx tm)
      | InferKind `Format -> InferFormat (check_format ctx tm)
      | InferType t -> InferExpr (check_expr ctx tm t, t)
      | InferExpr _ -> error tm.loc "expected annotation, found expression"
      | InferFormat f ->  InferExpr (check_expr ctx tm f.repr, f.repr)
      (*                                               ^^^^^^  ^^^^^^ TODO: preserve `repr` in core language *)
    end

    | RecordEmpty ->
      error tm.loc "ambiguous empty record"

    | RecordTy fs ->
      let rec go fs acc_fs =
        match fs with
        | [] -> InferType (RecordTy acc_fs)
        | (l, tm) :: fs  ->
          if LabelMap.mem l.data acc_fs then
            error l.loc (Format.asprintf "duplicate field labels `%s`" l.data)
          else
            (go [@tailcall]) fs (LabelMap.add l.data (check_type ctx tm) acc_fs)
      in
      go fs LabelMap.empty

    | RecordLit fs ->
      let rec go fs fs_e fs_t =
        match fs with
        | [] -> InferExpr (RecordLit fs_e, RecordTy fs_t)
        | (l, e) :: fs ->
          if LabelMap.mem l.data fs_e then
            error l.loc (Format.asprintf "duplicate field labels `%s`" l.data)
          else
            let e, t = infer_expr ctx e in
            (go [@tailcall]) fs (LabelMap.add l.data e fs_e) (LabelMap.add l.data t fs_t)
      in
      go fs LabelMap.empty LabelMap.empty

    | Action (f, (n, e)) ->
      let f = check_format ctx f in
      let e, t = infer_expr { ctx with locals = (n.data, f.repr) :: ctx.locals } e in
      let f = Core.{
        node = Map (t, (n.data, e), f);
        repr = t;
        info = f.info;
      } in
      InferFormat f

    | Union (f1, f2) ->
      let f1 = check_format ctx f1 in
      let f2 = check_format ctx f2 in
      equate_ty tm.loc f1.repr f1.repr;
      let f = Core.{
        node = Union (f1, f2);
        repr = f1.repr;
        info = Core.FormatInfo.union f1.info f2.info;
      } in
      InferFormat f

    | Range (start, stop) ->
      InferFormat (format_of_byte_set (byte_set_of_range start stop))

    | RecordFormat fs ->
      let rec go ctx seen fs : Core.format =
        match fs with
        | [] ->
          let is = List.init (List.length seen) Fun.id in
          let t = Core.RecordTy (is |> List.rev_map (List.nth ctx.locals) |> LabelMap.of_list) in
          let e = Core.RecordLit (is |> List.rev_map (fun i -> (List.nth seen i).data, Core.Local i) |> LabelMap.of_list) in
          {
            node = Pure (t, e);
            repr = t;
            info = Core.FormatInfo.empty;
          }
        | (l, f) :: fs ->
          if List.mem l seen then
            error l.loc (Format.sprintf "duplicate label in record format `%s`" l.data)
          else
            let f1 = check_format ctx f in
            let f2 = go { ctx with locals = (l.data, f1.repr) :: ctx.locals } (l :: seen) fs in
            {
              node = FlatMap (f1.repr, (l.data, f2), f1);
              repr = f2.repr;
              info = Core.FormatInfo.seq f1.info f2.info;
            }
      in
      InferFormat (go ctx [] fs)

    | Not tm ->
        let s =
          match tm.data with
          | IntLit i -> byte_set_of_int tm.loc i
          | Range (start, stop) -> byte_set_of_range start stop
          | _ -> error tm.loc "negation is only supported for integer and range formats"
        in
        InferFormat (format_of_byte_set (ByteSet.neg s))

    | IntLit i ->
      (* TODO: postpone elaboration *)
      InferExpr (ByteLit (byte_of_int tm.loc i), ByteTy)

    | Tuple tms ->
      (* TODO: postpone elaboration *)
      let es_ts = List.map (infer_expr ctx) tms in
      InferExpr (TupleLit (List.map fst es_ts), TupleTy (List.map snd es_ts))

    | Proj (e, `Label l) -> begin
      match infer ctx e with
      | InferExpr (e, RecordTy ts) -> begin
        match LabelMap.find_opt l.data ts with
        | Some t -> InferExpr (RecordProj (e, l.data), t)
        | None -> error l.loc (Format.sprintf "unknown field `%s`" l.data)
      end
      | InferFormat f when l.data = "Repr" ->
        InferType f.repr
        (*        ^^^^^^ TODO: preserve `Repr` in core language *)
      | _ -> error l.loc (Format.sprintf "unknown field `%s`" l.data)
    end

    | Proj (e, `Index i) -> begin
      match infer ctx e with
      | InferExpr (e, TupleTy ts) when i.data < List.length ts ->
        InferExpr (TupleProj (e, i.data), List.nth ts i.data)
      | _ -> error i.loc (Format.sprintf "unknown field `%i`" i.data)
    end


  (* Specialised elaboration functions *)

  and infer_expr (ctx : context) (tm : tm) : Core.expr * Core.ty =
    match infer ctx tm with
    | InferKind _ -> error tm.loc "expected expression, found kind"
    | InferExpr (e, t) -> e, t
    | InferType _ -> error tm.loc "expected expression, found type"
    | InferFormat _ -> error tm.loc "expected expression, found format"

  and infer_format (ctx : context) (tm : tm) : Core.format =
    match infer ctx tm with
    | InferKind _ -> error tm.loc "expected format, found kind"
    | InferType _ -> error tm.loc "expected format, found type"
    | InferExpr (_, _) -> error tm.loc "expected format, found expression"
    | InferFormat f -> f


  (** {2 Item traversal} *)

  (** Collect a dependency list for use when topologically sorting *)
  let collect_deps (items : item list) : (int * int list) list =
    let module StringMap = Map.Make (String) in
    let module StringSet = Set.Make (String) in

    let name_map =
      Seq.fold_lefti
        (fun map i item ->
          let n =
            match item with
            | FormatDef (n, _) -> n
            | TypeDef (n, _) -> n
            | Def (n, _, _) -> n
          in
          if StringMap.mem n.data map then
            error n.loc (Format.sprintf "the item name `%s` is defined multiple times" n.data)
          else
            StringMap.add n.data i map)
        StringMap.empty
        (List.to_seq items)
    in

    let rec tm_deps (locals : StringSet.t)  (t : tm) : int list =
      match t.data with
      | Name n when StringSet.mem n locals -> []
      | Name n -> StringMap.find_opt n name_map |> Option.to_list
      | Ann (tm, ann) -> tm_deps locals tm @ tm_deps locals ann
      | RecordEmpty -> []
      | RecordTy fs -> List.concat_map (fun (_, t) -> tm_deps locals t) fs
      | RecordLit fs -> List.concat_map (fun (_, e) -> tm_deps locals e) fs
      | RecordFormat fs ->
        let rec go locals fs =
          match fs with
          | [] -> []
          | (l, f) :: fs ->
            tm_deps locals f @ go (StringSet.add l.data locals) fs
        in
        go locals fs
      | IntLit _ -> []
      | Tuple ts -> List.concat_map (tm_deps locals) ts
      | Proj (e, _) -> tm_deps locals e
      | Action (f, (n, e)) -> tm_deps locals f @ tm_deps (StringSet.add n.data locals) e
      | Union (f1, f2) -> tm_deps locals f1 @ tm_deps locals f2
      | Not f -> tm_deps locals f
      | Range (_, _) -> []
    in

    items |> List.mapi @@ fun i item ->
      match item with
      | FormatDef (_, f) -> i, tm_deps StringSet.empty f
      | TypeDef (_, t) -> i, tm_deps StringSet.empty t
      | Def (_, None, e) -> i, tm_deps StringSet.empty e
      | Def (_, Some t, e) -> i, tm_deps StringSet.empty t @ tm_deps StringSet.empty e

  let check_program (is : item list) : Core.program =
    let check_item (ctx : context) (i : item) : string * Core.item =
      match i with
      | FormatDef (n, f) -> n.data, Format (check_format ctx f)
      | TypeDef (n, t) -> n.data, Type (check_type ctx t)
      | Def (n, None, body) -> begin
        match infer ctx body with
        | InferKind _ -> error n.loc "kind definitions are not supported"
        | InferType t -> n.data, Type t
        | InferExpr (e, t) -> n.data, Expr (e, t)
        | InferFormat f ->  n.data, Format f
      end
      | Def (n, Some ann, body) -> begin
        match infer ctx ann with
        | InferKind `Type -> n.data, Type (check_type ctx body)
        | InferKind `Format -> n.data, Format (check_format ctx body)
        | InferType t -> n.data, Expr (check_expr ctx body t, t)
        | InferExpr _ -> error body.loc "expected annotation, found expression"
        | InferFormat f ->  n.data, Expr (check_expr ctx body f.repr, f.repr)
        (*                                                    ^^^^^^  ^^^^^^ TODO: preserve `repr` in core language *)
      end
    in

    let[@tail_mod_cons] rec go ctx order =
      match order with
      | i :: order ->
        let n, i = check_item ctx (List.nth is i) in
        go { ctx with items = (n, i) :: ctx.items } order
      | [] ->
        Core.{ items = List.rev ctx.items }
    in

    (* TODO: Sort with strongly connected components, elaborating to fixedpoints *)
    match Tsort.sort (collect_deps is) with
    | Tsort.Sorted order -> go { items = []; locals = [] } order
    | Tsort.ErrorCycle _ -> failwith "TODO: cyclic items" (* TODO: Better error *)

end
