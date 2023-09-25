module Lookahead = struct

  type t = ByteSet.t list

  let pp_print fmt l =
    Format.fprintf fmt "@[[%a]@]"
      (Format.pp_print_list
        ByteSet.pp_print
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")) l

  let rec alt (l0 : t) (l1 : t) : t =
    match l0, l1 with
    | [], _ | _, [] -> []
    | s0 :: l0, s1 :: l1 ->
        ByteSet.union s0 s1 :: alt l0 l1

  let cat (l0 : t) (l1 : t) : t =
    l0 @ l1

end

module Format = struct
  (** Formats with an arbitrary amount of lookahead *)

  type t =
    | Zero
    | Unit
    | Byte of ByteSet.t
    | Alt of t * t
    | Cat of t * t
    | Repeat of t * t
    [@@warning "-unused-constructor"]

  let ( let* ) = Option.bind

  (** Returns [Some] if the format matches the supplied lookahead, along with
      any remaining unmatched lookahead. *)
  let rec can_match_lookahead (input : Lookahead.t) : t -> Lookahead.t option =
    function
    | Zero -> None
    | Unit -> Some input
    | Byte s ->
        begin match input with
        | s' :: _ when ByteSet.disjoint s s' -> None
        | _ :: input -> Some input
        | [] -> Some []
        end
    | Alt (f0, f1) ->
        begin match can_match_lookahead input f0 with
        | Some _ as opt -> opt
        | None -> can_match_lookahead input f1
        end
    | Cat (f0, f1) ->
        let* input = can_match_lookahead input f0 in
        let* input = can_match_lookahead input f1 in
        Some input
    | Repeat (f0, f1) ->
        let rec go input =
          match can_match_lookahead input f0 with
          | Some input -> go input
          | None -> Some input
        in
        let* input = go input in
        can_match_lookahead input f1

  (** Return [n] bytes of lookahead for the format *)
  let rec lookahead (n : int) : t -> Lookahead.t option =
    function
    | Zero -> None
    | Unit -> Some []
    | Byte s -> Some [s]
    | Alt (f0, f1) | Repeat(f0, f1) ->
        let* l0 = lookahead n f0 in
        let* l1 = lookahead n f1 in
        Some (Lookahead.alt l0 l1)
    | Cat (f0, f1) ->
        let* l0 = lookahead n f0 in
        let n' = n - List.length l0 in
        if n' <= 0 then Some l0 else
          let* l1 = lookahead n' f1 in
          Some (Lookahead.cat l0 l1)

  (** Return [n] bytes of lookahead for [a], ensuring that it does not match [b] *)
  let disjoint_lookahead (n : int) (f0 : t) (f1 : t) : Lookahead.t option =
    let* l0 = lookahead n f0 in
    match can_match_lookahead l0 f1 with
    | None -> Some l0
    | Some _ -> None

end

module DetFormat = struct
  (** Deterministic formats with a fixed amount of lookahead *)

  type t =
    | Zero
    | Unit
    | Byte of ByteSet.t
    | If of Lookahead.t * t * t
    | Cat of t * t
    | While of Lookahead.t * t
    | Until of Lookahead.t * t

  let rec pp_print fmt =
    let module Format = Stdlib.Format in
    function
    | Zero -> Format.fprintf fmt "zero"
    | Unit -> Format.fprintf fmt "unit"
    | Byte s -> Format.fprintf fmt "%a" ByteSet.pp_print s
    | If (l, f0, f1) ->
        Format.fprintf fmt "if@ %a@ @[(%a)@]@ @[(%a)@]"
          Lookahead.pp_print l
          pp_print f0
          pp_print f1
    | Cat (f0, f1) ->
        Format.fprintf fmt "%a,@ %a"
          pp_print f0
          pp_print f1
    | While (l, f) ->
        Format.fprintf fmt "while@ %a@ @[(%a)@]"
          Lookahead.pp_print l
          pp_print f
    | Until (l, f) ->
        Format.fprintf fmt "until@ %a@ @[(%a)@]"
          Lookahead.pp_print l
          pp_print f

end

(** Compile a format to a deterministic format with [n] bytes of lookahead. *)
let rec compile (n : int) : Format.t -> (DetFormat.t, string) result =
  let ( let* ) = Result.bind in

  function
  | Zero -> Ok Zero
  | Unit -> Ok Unit
  | Byte s -> Ok (Byte s)
  | Alt (f0, f1) ->
      begin match Format.disjoint_lookahead n f0 f1 with
      | Some l ->
          let* f0 = compile n f0 in
          let* f1 = compile n f1 in
          Ok (DetFormat.If (l, f0, f1))
      | None ->
          begin match Format.disjoint_lookahead n f1 f0 with
          | Some l ->
              let* f0 = compile n f0 in
              let* f1 = compile n f1 in
              Ok (DetFormat.If (l, f1, f0))
          | None -> Error "cannot find valid lookahead for alt"
          end
      end
  | Cat (f0, f1) ->
      let* f0 = compile n f0 in
      let* f1 = compile n f1 in
      Ok (DetFormat.Cat (f0, f1))
  | Repeat (f0, f1) ->
      begin match Format.disjoint_lookahead n f0 f1 with
      | Some l ->
          let* f0 = compile n f0 in
          let* f1 = compile n f1 in
          Ok (DetFormat.Cat (While (l, f0), f1))
      | None ->
          begin match Format.disjoint_lookahead n f1 f0 with
          | Some l ->
              let* f0 = compile n f0 in
              let* f1 = compile n f1 in
              Ok (DetFormat.Cat (Until (l, f0), f1))
          | None -> Error "cannot find valid lookahead for repeat"
          end
      end


let () =
  let is i : Format.t = Byte ByteSet.(singleton i) in
  let not i : Format.t = Byte ByteSet.(singleton i |> neg) in

  let ( <|> ) f0 f1 : Format.t = Alt (f0, f1) in
  let ( <+> ) f0 f1 : Format.t = Cat (f0, f1) in
  let repeat f0 f1 : Format.t = Repeat (f0, f1) in

  (*
      repeat (^0xFF | (0xFF, 0x00)) (0xFF, 0xD9)
  *)
  let jpeg : Format.t =
    let marker tag = is '\xFF' <+> is tag in

    let soi = marker '\xD8' in (* Start of image *)
    let eoi = marker '\xD9' in (* End of image *)

    let image_data = not '\xFF' <|> (is '\xFF' <+> is '\x00') in

    soi (* TODO: segments *) <+> repeat image_data eoi
  in

  match compile 2 jpeg with
  | Ok det_jpeg ->
      (*
        0xFF, 0xD8, until [0xFF, 0xD9] (if [^0xFF] (^0xFF) (0xFF, 0x00)), 0xFF, 0xD9
      *)
      Stdlib.Format.printf "@[%a@]\n"
        DetFormat.pp_print det_jpeg
  | Error error ->
      failwith error
