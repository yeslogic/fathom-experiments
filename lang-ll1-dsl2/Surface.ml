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

  (* An elaborated type *)
  type _ elab_ty =
    | Kind : [`Kind] elab_ty
    | TypeKind : [`TypeKind] elab_ty
    | FormatKind : [`FormatKind] elab_ty
    | Type : Core.ty -> Core.ty elab_ty

  (* An elaborated term *)
  type _ elab_tm =
    | TypeKind : [`Kind] elab_tm
    | FormatKind : [`Kind] elab_tm
    | Type : Core.ty -> [`TypeKind] elab_tm
    | Format : Core.format -> [`FormatKind] elab_tm
    | Expr : Core.expr -> Core.ty elab_tm

  type ann_tm =
    | AnnTm : 'ann elab_tm * 'ann elab_ty -> ann_tm

  let expect_kind loc tm =
    match tm with
    | AnnTm (TypeKind, _) -> TypeKind
    | AnnTm (FormatKind, _) -> FormatKind
    | AnnTm (Type _, _) -> error loc "expected kind, found type"
    | AnnTm (Expr _, _) -> error loc "expected kind, found expression"
    | AnnTm (Format _, _) -> error loc "expected kind, found format"

  let expect_type loc tm =
    match tm with
    | AnnTm ((TypeKind | FormatKind), _) -> error loc "expected type, found kind"
    | AnnTm (Type t, _) -> Type t
    | AnnTm (Expr _, _) -> error loc "expected type, found expression"
    | AnnTm (Format _, _) -> error loc "expected type, found format"

  let expect_expr loc tm =
    match tm with
    | AnnTm ((TypeKind | FormatKind), Kind) -> error loc "expected expression, found kind"
    | AnnTm (Expr e, Type t) -> e, t
    | AnnTm (Type _, TypeKind) -> error loc "expected expression, found type"
    | AnnTm (Format _, FormatKind) -> error loc "expected expression, found format"

  let expect_format loc tm =
    match tm with
    | AnnTm ((TypeKind | FormatKind), _) -> error loc "expected format, found kind"
    | AnnTm (Type _, _) -> error loc "expected format, found type"
    | AnnTm (Expr _, _) -> error loc "expected format, found expression"
    | AnnTm (Format f, _) -> Format f


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


  (** Elaborate a surface term into a core term, given an expected type. *)
  let rec check : type ann. context -> tm -> ann elab_ty -> ann elab_tm =
    fun ctx tm ty ->
      match tm.data with
      (* Empty records *)
      | RecordEmpty -> begin
        match ty with
        | Kind -> error tm.loc "expected kind, found empty record"
        | TypeKind -> Type (RecordTy LabelMap.empty)
        | FormatKind ->
          Format Core.{
            node = Pure (RecordTy LabelMap.empty, RecordLit LabelMap.empty);
            repr = RecordTy LabelMap.empty;
            info = FormatInfo.empty;
          }
        | Type t ->
          equate_ty tm.loc t (RecordTy LabelMap.empty);
          Expr (RecordLit LabelMap.empty)
      end

      (* Tuples *)
      | Tuple tms -> begin
        match ty with
        | Kind -> error tm.loc "expected kind, found tuple"
        | TypeKind -> Type (TupleTy (List.map (check_type ctx) tms))
        | FormatKind ->
          let fs = List.map (check_format ctx) tms in
          Format Core.{
            node = Seq fs;
            repr = TupleTy (List.map (fun f -> f.repr) fs);
            info = List.fold_right (fun f acc -> FormatInfo.seq f.info acc) fs FormatInfo.empty;
          }
        | Type t -> begin
          match t with
          | TupleTy ts ->
            let rec go tms ts =
              match tms, ts with
              | [], [] -> []
              | tm :: tms, t :: ts -> check_expr ctx tm t :: go tms ts
              | _, _ -> error tm.loc "unexpected number of elements in tuple"
            in
            Expr (TupleLit (go tms ts))
          | t ->
            error tm.loc
              (Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %a@]@ @[found: tuple@]@]"
                Core.pp_print_ty t)
        end

      end

      (* Integer literals *)
      | IntLit i -> begin
        match ty with
        | FormatKind -> Format (format_of_byte_set (byte_set_of_int tm.loc i))
        | Type ByteTy -> Expr (ByteLit (byte_of_int tm.loc i))
        | _ -> error tm.loc "unexpected integer literal"
      end

      (* Conversion *)
      | _ -> begin
        match ty with
        | Kind -> infer ctx tm |> expect_kind tm.loc
        | TypeKind -> infer ctx tm |> expect_type tm.loc
        | FormatKind -> infer ctx tm |> expect_format tm.loc
        | Type t ->
          let e', t' = infer_expr ctx tm in
          equate_ty tm.loc t t';
          Expr e'
      end

  (** Elaborate a surface term into a core term, inferring its type. *)
  and infer (ctx : context) (tm : tm) : ann_tm =
    match tm.data with
    | Name n -> begin
      match lookup_local ctx n with
      | Some (e, t) -> AnnTm (Expr e, Type t)
      | None -> begin
        match List.assoc_opt n ctx.items with
        | Some (Type _) -> AnnTm (Type (Item n), TypeKind)
        | Some (Format f) -> AnnTm (Format ({ f with node = Item n }), FormatKind)
        | Some (Expr (_, ty)) -> AnnTm (Expr (Item n), Type ty)
        | None when n = "Type" -> AnnTm (TypeKind, Kind)
        | None when n = "Format" -> AnnTm (FormatKind, Kind)
        | None when n = "Byte" -> AnnTm (Type ByteTy, TypeKind)
        | None -> error tm.loc (Format.asprintf "unbound name `%s`" n)
      end
    end

    | RecordEmpty ->
      error tm.loc "ambiguous empty record"

    | RecordTy fs ->
      let rec go fs acc_fs =
        match fs with
        | [] -> AnnTm (Type (RecordTy acc_fs), TypeKind)
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
        | [] -> AnnTm (Expr (RecordLit fs_e), Type (RecordTy fs_t))
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
      AnnTm (Format f, FormatKind)

    | Union (f1, f2) ->
      let f1 = check_format ctx f1 in
      let f2 = check_format ctx f2 in
      equate_ty tm.loc f1.repr f1.repr;
      let f = Core.{
        node = Union (f1, f2);
        repr = f1.repr;
        info = Core.FormatInfo.union f1.info f2.info;
      } in
      AnnTm (Format f, FormatKind)

    | Range (start, stop) ->
      AnnTm (Format (format_of_byte_set (byte_set_of_range start stop)), FormatKind)

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
      AnnTm (Format (go ctx [] fs), FormatKind)

    | Not tm ->
        let s =
          match tm.data with
          | IntLit i -> byte_set_of_int tm.loc i
          | Range (start, stop) -> byte_set_of_range start stop
          | _ -> error tm.loc "negation is only supported for integer and range formats"
        in
        AnnTm (Format (format_of_byte_set (ByteSet.neg s)), FormatKind)

    | IntLit i ->
      (* TODO: postpone elaboration *)
      AnnTm (Expr (ByteLit (byte_of_int tm.loc i)), Type ByteTy)

    | Tuple tms ->
      (* TODO: postpone elaboration *)
      let es_ts = List.map (infer_expr ctx) tms in
      AnnTm (Expr (TupleLit (List.map fst es_ts)), Type (TupleTy (List.map snd es_ts)))

    | Proj (e, `Label l) -> begin
      match infer ctx e with
      | AnnTm (Expr e, Type (RecordTy ts)) -> begin
        match LabelMap.find_opt l.data ts with
        | Some t -> AnnTm (Expr (RecordProj (e, l.data)), Type t)
        | None -> error l.loc (Format.sprintf "unknown field `%s`" l.data)
      end

      | AnnTm (Format f, FormatKind) when l.data = "Repr" ->
        AnnTm (Type f.repr, TypeKind)
        (*          ^^^^^^ TODO: preserve `Repr` in core language *)

      | _ -> error l.loc (Format.sprintf "unknown field `%s`" l.data)
    end

    | Proj (e, `Index i) -> begin
      match infer ctx e with
      | AnnTm (Expr e, Type (TupleTy ts)) when i.data < List.length ts ->
        AnnTm (Expr (TupleProj (e, i.data)), Type (List.nth ts i.data))

      | _ -> error i.loc (Format.sprintf "unknown field `%i`" i.data)
    end

  (* Specialised elaboration functions *)

  and check_expr (ctx : context) (tm : tm) (t : Core.ty) : Core.expr =
    let Expr e = check ctx tm (Type t) in e

  and check_type (ctx : context) (tm : tm) : Core.ty =
    let Type t = check ctx tm TypeKind in t

  and check_format (ctx : context) (tm : tm) : Core.format =
    let Format f = check ctx tm FormatKind in f

  and infer_expr (ctx : context) (tm : tm) : Core.expr * Core.ty =
    infer ctx tm |> expect_expr tm.loc


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
      | Def (n, None, body) -> let e, t = infer_expr ctx body in n.data, Expr (e, t)
      | Def (n, Some ann, body) -> begin
        match infer ctx ann with
        | AnnTm (TypeKind, _) -> n.data, Type (check_type ctx body)
        | AnnTm (FormatKind, _) -> n.data, Format (check_format ctx body)
        | AnnTm (Type t, _) -> n.data, Expr (check_expr ctx body t, t)
        | AnnTm (Expr _, _) -> error body.loc "expected annotation, found expression"
        | AnnTm (Format f, _) ->  n.data, Expr (check_expr ctx body f.repr, f.repr)
        (*                                                          ^^^^^^  ^^^^^^ TODO: preserve `repr` in core language *)
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
