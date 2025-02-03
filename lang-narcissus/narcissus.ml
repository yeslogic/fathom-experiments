module Expr = struct

  type t =
    | Var of int
    | RecordLit of (string * t) list
    | RecordProj of t * string
    | ByteLit of char
    | BytesLit of t list
    | BytesAppend of t * t

end

module Pred = struct

  type t =
    | And of t * t
    | Or of t * t
    | Eq of Expr.t * Expr.t

end

module Format = struct

  type t =
    | Byte
    | Seq of t * t
    | Proj of t * string
    (* | Union of t * t *)
    (* | Restrict of Pred.t * t *)
    | Empty

  let rec seq (formats : t list) : t =
    match formats with
    | [] -> Empty
    | format :: formats -> Seq (format, seq formats)

end

module Decoder = struct

  type t =
    | Byte
    | Pure of Expr.t
    | Bind of t * Expr.t
    | Fail of t

  let compile (_pred : Pred.t) (format : Format.t) : t =
    (* Based on Figure 5 from the Narcissus paper *)
    match format with
    | Byte ->
        Byte

    (* DecCompose *)
    | Proj (_, _) ->
        failwith "TODO"

    (* DecSeqProj *)
    | Seq (Proj _, _) ->
        failwith "TODO"

    (* DecSeq *)
    | Seq (_, _) ->
        failwith "TODO"

    (* DecDone *)
    | Empty ->
        (* if (pred src) then pure src else fail *)
        failwith "TODO"

end

module Encoder = struct

  type t =
    | Byte
    | Pure of Expr.t
    | Bind of t * t
    | Fail of t

  type neu =
    | Var of int
    | RecordProj of neu * string

  let ( let@ ) = ( @@ )

  let rec quote_neu (size : int) (neu : neu) : Expr.t =
    match neu with
    | Var level -> Var (size - level - 1)
    | RecordProj (neu, label) -> RecordProj (quote_neu size neu, label)

  let rec compile (size : int) (src : neu) (format : Format.t) : t =
    (* Based on Figure 3 from the Narcissus paper *)
    match format with
    | Byte ->
        Pure (BytesLit [quote_neu size src])

    (* EncComp *)
    | Proj (format, label) ->
        compile size (RecordProj (src, label)) format

    (* EncSeq *)
    | Seq (format1, format2) ->
        (* TODO: Check format relations? Might be important for threading through state *)
        let@ size, x = compile_bind size src format1 in
        let@ size, y = compile_bind size src format2 in
        Pure (BytesAppend (quote_neu size x, quote_neu size y))

    (*
      (* EncRest *)
      | Restrict (pred, format) ->
          let src = quote_neu size src in
          If (PredApp (pred, src), compile size src format, Fail)

      (* EncUnion *)
      | Union (format1, format2) ->
          let encode1 = compile size src format1
          and encode2 = compile size src format2 in
          (* How do we choose between [format1] and [format2]? *)
          failwith "TODO"
    *)

    (* EncEmpty *)
    | Empty ->
        Pure (BytesLit [])

  and compile_bind (size : int) (src : neu) (format : Format.t) (body : int * neu -> t) : t =
    Bind ((* x, *) compile size src format,
      body (size + 1, Var size))

end

module Examples = struct

  let sensor_msg : Format.t = Format.seq [
    Proj (Byte, "station-id");
    Proj (Byte, "data");
  ]

  let sensor_msg_decoder =
    Decoder.compile (failwith "TODO") sensor_msg

  let sensor_msg_encoder =
    Encoder.compile (failwith "TODO") (failwith "TODO") sensor_msg

end
