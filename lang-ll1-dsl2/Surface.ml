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

module Ty = struct

  type t =
    node located

  and node =
    | Name of string
    | Record of (string located * t) list
    | Tuple of t list

end

module Expr = struct

  type t =
    node located

  and node =
    | Name of string
    | RecordLit of (string located * t) list
    | TupleLit of t list
    | IntLit of int
    | ProjLabel of t * string located
    | ProjIndex of t * int located

end

module Format = struct

  type t =
    node located

  and node =
    | Name of string
    | Action of t * (binder * Expr.t)
    | Union of t * t
    | Not of t
    | Int of int
    | Range of bound * bound
    | Record of (string located * t) list
    | Tuple of t list

  and bound =
    | Open
    | Inclusive of int located
    | Exclusive of int located

end

type item =
  | FormatDef of binder * Format.t
  | TypeDef of binder * Ty.t
  | ExprDef of binder * Ty.t option * Expr.t

type program =
  item list


(** {1 Elaboration} *)

module Elab = struct

  exception Error of loc * string
  exception Bug of loc * string

  let error loc msg = raise (Error (loc, msg))
  let bug loc msg = raise (Bug (loc, msg))

  (* An elaborated type *)
  type _ elab_ty =
    | Sort : [`Sort] elab_ty
    | Kind : [`Type | `Format] -> [`Type | `Format] elab_ty
    | Type : Core.ty -> Core.ty elab_ty

  (* An elaborated term *)
  type _ elab_tm =
    | Kind : [`Type | `Format] -> [`Sort] elab_tm
    | Type : Core.ty -> [> `Type] elab_tm
    | Format : Core.format -> [> `Format] elab_tm
    | Expr : Core.expr -> Core.ty elab_tm

  type ann_tm =
    | AnnTm : 'ann elab_tm * 'ann elab_ty -> ann_tm


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
      error loc (Stdlib.Format.asprintf "integer `%i` is outside the range `0..255`" i)

  let byte_set_of_int loc i =
    ByteSet.singleton (byte_of_int loc i)

  let byte_set_of_range (start : Format.bound) (stop : Format.bound) =
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


  (** Bidirectional type checking *)

  let equate_ty (loc : loc) (ty1 : Core.ty) (ty2 : Core.ty) =
    if ty1 = ty2 then () else
      error loc
        (Stdlib.Format.asprintf "@[<v 2>@[mismatched types:@]@ @[expected: %a@]@ @[found: %a@]@]"
          Core.pp_print_ty ty1
          Core.pp_print_ty ty2)

  let infer_name (ctx : context) (loc : loc) (n : string) : ann_tm =
    let local = ctx.locals |> List.find_mapi @@ fun i (n', t) ->
      if n = n' then Some (Core.Local i, t) else None
    in
    match local with
    | Some (e, t) -> AnnTm (Expr e, Type t)
    | None -> begin
      match List.assoc_opt n ctx.items with
      | Some (Type _) -> AnnTm (Type (Item n), Kind `Type)
      | Some (Format f) -> AnnTm (Format ({ f with node = Item n }), Kind `Format)
      | Some (Expr (_, ty)) -> AnnTm (Expr (Item n), Type ty)
      | None when n = "Type" -> AnnTm (Kind `Type, Sort)
      | None when n = "Format" -> AnnTm (Kind `Format, Sort)
      | None when n = "Byte" -> AnnTm (Type ByteTy, Kind `Type)
      | None -> error loc (Stdlib.Format.asprintf "unbound name `%s`" n)
    end

  let rec check_ty (ctx : context) (t : Ty.t) : Core.ty =
    match t.data with
    | Name n -> begin
      match infer_name ctx t.loc n with
      | AnnTm (Type t, _) -> t
      | AnnTm (Expr _, _) -> error t.loc "expected type, found expression"
      | AnnTm (Format _, _) -> error t.loc "expected type, found format"
      | AnnTm (Kind _, _) -> error t.loc "expected type, found kind"
    end

    | Record fs ->
      RecordTy
        (List.fold_left
          (fun fs (l, t) ->
            if Core.LabelMap.mem l.data fs then
              error l.loc (Stdlib.Format.asprintf "duplicate field labels `%s`" l.data)
            else
              Core.LabelMap.add l.data (check_ty ctx t) fs)
          Core.LabelMap.empty
          fs)

    | Tuple ts ->
      TupleTy (List.map (check_ty ctx) ts)

  let rec check_expr (ctx : context) (e : Expr.t) (t : Core.ty) : Core.expr =
    match e.data with
    | _ ->
      let e', t' = infer_expr ctx e in
      equate_ty e.loc t t';
      e'

  and infer_expr (ctx : context) (e : Expr.t) : Core.expr * Core.ty =
    match e.data with
    | Name n -> begin
      match infer_name ctx e.loc n with
      | AnnTm (Type _, _) -> error e.loc "expected expression, found type"
      | AnnTm (Expr e, Type t) -> e, t
      | AnnTm (Format _, _) -> error e.loc "expected type, found format"
      | AnnTm (Kind _, _) -> error e.loc "expected type, found kind"
    end

    | RecordLit fs ->
      let rec go fs_e fs_t fs =
        match fs with
        | [] -> fs_e, fs_t
        | (l, _) :: _ when Core.LabelMap.mem l.data fs_e ->
          error l.loc (Stdlib.Format.asprintf "duplicate field labels `%s`" l.data)
        | (l, e) :: fs ->
          let e, t = infer_expr ctx e in
          go (Core.LabelMap.add l.data e fs_e) (Core.LabelMap.add l.data t fs_t) fs
      in
      let fs_e, fs_t = go Core.LabelMap.empty Core.LabelMap.empty fs in
      RecordLit fs_e, RecordTy fs_t

    | TupleLit es ->
      let es_ts = List.map (infer_expr ctx) es in
      TupleLit (List.map fst es_ts), TupleTy (List.map snd es_ts)

    | IntLit i ->
      ByteLit (byte_of_int e.loc i), ByteTy

    | ProjLabel (e, l) -> begin
      match infer_expr ctx e with
      | e, RecordTy ts -> begin
        match Core.LabelMap.find_opt l.data ts with
        | Some t -> RecordProj (e, l.data), t
        | None -> error l.loc (Stdlib.Format.sprintf "unknown field `%s`" l.data)
      end
      | _ -> error l.loc (Stdlib.Format.sprintf "unknown field `%s`" l.data)
    end

    | ProjIndex (e, i) -> begin
      match infer_expr ctx e with
      | e, TupleTy ts when i.data < List.length ts -> TupleProj (e, i.data), List.nth ts i.data
      | _ -> error i.loc (Stdlib.Format.sprintf "unknown field `%i`" i.data)
    end

  let rec check_format (ctx : context) (f : Format.t) : Core.format =
    let format_of_byte_set s = Core.{
      node = Byte s;
      repr = ByteTy;
      info = Core.FormatInfo.byte s;
    } in

    match f.data with
    | Name n -> begin
      match infer_name ctx f.loc n with
      | AnnTm (Type _, _) -> error f.loc "expected format, found type"
      | AnnTm (Expr _, _) -> error f.loc "expected type, found expression"
      | AnnTm (Format f, _) -> f
      | AnnTm (Kind _, _) -> error f.loc "expected type, found kind"
    end

    | Action (f, (n, e)) ->
      let f = check_format ctx f in
      let e, t = infer_expr { ctx with locals = (n.data, f.repr) :: ctx.locals } e in
      {
        node = Map (t, (n.data, e), f);
        repr = t;
        info = f.info;
      }

    | Union (f1, f2) ->
      let f1 = check_format ctx f1 in
      let f2 = check_format ctx f2 in
      equate_ty f.loc f1.repr f1.repr;
      {
        node = Union (f1, f2);
        repr = f1.repr;
        info = Core.FormatInfo.union f1.info f2.info;
      }

    | Not f ->
      let s =
        match f.data with
        | Int i -> byte_set_of_int f.loc i
        | Range (start, stop) -> byte_set_of_range start stop
        | _ -> error f.loc "negation is only supported for integer and range formats"
      in
      format_of_byte_set (ByteSet.neg s)

    | Int i ->
      format_of_byte_set (byte_set_of_int f.loc i)

    | Range (start, stop) ->
      format_of_byte_set (byte_set_of_range start stop)

    | Record fs ->
      let rec go ctx seen fs : Core.format =
        match fs with
        | [] ->
          let is = Seq.ints 0 |> Seq.take (List.length seen) in (* FIXME: reverse *)
          let t = Core.RecordTy (is |> Seq.map (List.nth ctx.locals) |> Core.LabelMap.of_seq) in
          let e = Core.TupleLit (is |> Seq.map (fun i -> Core.Local i) |> List.of_seq) in
          Core.{
            node = Pure (t, e);
            repr = t;
            info = FormatInfo.empty;
          }
        | (l, _) :: _ when List.mem l seen ->
          error l.loc (Stdlib.Format.sprintf "duplicate label in record format `%s`" l.data)
        | (l, f) :: fs ->
          let f1 = check_format ctx f in
          let f2 = go { ctx with locals = (l.data, f1.repr) :: ctx.locals } (l :: seen) fs in
          Core.{
            node = FlatMap (f1.repr, (l.data, f2), f1);
            repr = f2.repr;
            info = FormatInfo.seq f1.info f2.info;
          }
      in
      go ctx [] fs

    | Tuple fs ->
      let fs = List.map (check_format ctx) fs in
      Core.{
        node = Seq fs;
        repr = TupleTy (List.map (fun f -> f.repr) fs);
        info = List.fold_right (fun f acc -> FormatInfo.seq f.info acc) fs FormatInfo.empty;
      }

  (** Collect a dependency list to be used when topologically sorting *)
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
            | ExprDef (n, _, _) -> n
          in
          if StringMap.mem n.data map then
            error n.loc (Stdlib.Format.sprintf "the item name `%s` is defined multiple times" n.data)
          else
            StringMap.add n.data i map)
        StringMap.empty
        (List.to_seq items)
    in

    let name_deps (locals : StringSet.t) (n : string) =
      if StringSet.mem n locals then [] else
        StringMap.find_opt n name_map |> Option.to_list
    in

    let rec type_deps (t : Ty.t) : int list =
      match t.data with
      | Name n -> StringMap.find_opt n name_map |> Option.to_list
      | Record fs -> List.concat_map (fun (_, t) -> type_deps t) fs
      | Tuple ts -> List.concat_map type_deps ts
    in

    let rec expr_deps (locals : StringSet.t) (e : Expr.t) : int list =
      match e.data with
      | Name n -> name_deps locals n
      | RecordLit fs -> List.concat_map (fun (_, e) -> expr_deps locals e) fs
      | TupleLit es -> List.concat_map (expr_deps locals) es
      | IntLit _ -> []
      | ProjLabel (e, _) -> expr_deps locals e
      | ProjIndex (e, _) -> expr_deps locals e
    in

    let rec format_deps (locals : StringSet.t) (f : Format.t) : int list =
      match f.data with
      | Name n -> name_deps locals n
      | Action (f, (n, e)) -> format_deps locals f @ expr_deps (StringSet.add n.data locals) e
      | Union (f1, f2) -> format_deps locals f1 @ format_deps locals f2
      | Not f -> format_deps locals f
      | Int _ -> []
      | Range (_, _) -> []
      | Record fs ->
        let rec go locals fs =
          match fs with
          | [] -> []
          | (l, f) :: fs ->
            format_deps locals f @ go (StringSet.add l.data locals) fs
        in
        go locals fs
      | Tuple fs -> List.concat_map (format_deps locals) fs
    in

    items |> List.mapi @@ fun i item ->
      match item with
      | FormatDef (_, f) -> i, format_deps StringSet.empty f
      | TypeDef (_, t) -> i, type_deps t
      | ExprDef (_, None, e) -> i, expr_deps StringSet.empty e
      | ExprDef (_, Some t, e) -> i, type_deps t @ expr_deps StringSet.empty e

  let check_program (is : item list) : Core.program =
    let check_item (ctx : context) (i : item) : string * Core.item =
      match i with
      | FormatDef (n, f) -> n.data, Format (check_format ctx f)
      | TypeDef (n, t) -> n.data, Type (check_ty ctx t)
      | ExprDef (n, None, e) -> let e, t = infer_expr ctx e in n.data, Expr (e, t)
      | ExprDef (n, Some t, e) -> let t = check_ty ctx t in n.data, Expr (check_expr ctx e t, t)
    in
    let[@tail_mod_cons] rec go ctx order =
      match order with
      | i :: order ->
        let n, i = check_item ctx (List.nth is i) in
        go { ctx with items = (n, i) :: ctx.items } order

      | [] ->
        Core.{ items = List.rev ctx.items }
    in
    match Tsort.sort (collect_deps is) with
    | Tsort.Sorted order -> go { items = []; locals = [] } order
    | Tsort.ErrorCycle _ -> failwith "TODO: cyclic items" (* TODO: Better error *)

end
