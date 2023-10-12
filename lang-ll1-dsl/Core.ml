type ty =
  | UnitTy
  | ByteTy
  | PairTy of ty * ty

type expr =
  | UnitIntro
  | ByteIntro of char
  | PairIntro of expr * expr

type format =
  | Item of int
  | Unit
  | Byte of ByteSet.t
  | Cat of format * format
  | Alt of format * format

type program = {
  items : (string * format) list;
}


(* Pretty printing *)

let rec pp_print_format item_names ppf format =
  pp_print_alt_format item_names ppf format

and pp_print_alt_format item_names ppf format =
  match format with
  | Alt (f0, f1) ->
      Format.fprintf ppf "@[%a@]@ |@ %a"
        (pp_print_cat_format item_names) f0
        (pp_print_alt_format item_names) f1
  | f -> Format.fprintf ppf "%a" (pp_print_cat_format item_names) f

and pp_print_cat_format item_names ppf format =
  match format with
  | Cat (f0, f1) ->
      Format.fprintf ppf "@[%a,@]@ %a"
        (pp_print_atomic_format item_names) f0
        (pp_print_cat_format item_names) f1
  | f -> Format.fprintf ppf "%a" (pp_print_atomic_format item_names) f

and pp_print_atomic_format item_names ppf format =
  match format with
  | Item level -> Format.fprintf ppf "%s" (List.nth item_names (List.length item_names - level - 1))
  | Unit -> Format.fprintf ppf "()"
  | Byte s -> Format.fprintf ppf "@[%a@]" ByteSet.pp_print s
  | f -> Format.fprintf ppf "(%a)" (pp_print_format item_names) f

let pp_print_program ppf program =
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
  go [] ppf program.items


module Refiner = struct

  type item_context = string list

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

    let def_format (name, format) (body : item_var -> is_program) : is_program =
      fun items ->
        let level = List.length items in
        let format = format items in
        let program = body { level } (name :: items) in
        { items = (name, format) :: program.items }

  end

  module Format = struct

    let unit : is_format =
      fun _ -> Unit

    let item (var : item_var) : is_format =
      fun items ->
        match List.nth_opt items var.level with
        | Some _ -> Item var.level
        | None -> failwith "unbound item variable"

    let byte (s : ByteSet.t) : is_format =
      fun _ ->
        Byte s

    let cat (f0 : is_format) (f1 : is_format) : is_format =
      fun items ->
        let f0 = f0 items in
        let f1 = f1 items in
        (* TODO: check separate *)
        Cat (f0, f1)

    let alt (f0 : is_format) (f1 : is_format) : is_format =
      fun items ->
        let f0 = f0 items in
        let f1 = f1 items in
        (* TODO: check non-overlapping *)
        Alt (f0, f1)

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

  let run program f input pos =
    let size = List.length program.items in
    let rec go f pos =
      match f with
      | Item level -> begin
          match List.nth_opt program.items (size - level - 1) with
          | Some (_, f) -> go f pos
          | None -> failwith "unbound item variable"
      end
      | Unit -> pos, UnitIntro
      | Byte s -> begin
          match get_byte input pos with
          | Some c when ByteSet.mem c s -> pos + 1, ByteIntro c
          | _ -> raise (DecodeFailure pos)
      end
      | Cat (f0, f1) ->
          let (pos, e0) = go f0 pos in
          let (pos, e1) = go f1 pos in
          pos, PairIntro (e0, e1)
      | Alt (f0, f1) ->
          (* TODO: Use FIRST *)
          try go f0 pos with
          | DecodeFailure _ -> go f1 pos
    in
    go f pos

end
