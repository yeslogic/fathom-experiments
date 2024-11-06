(** A linear-time parser combinator library for byte strings.

    - {{:https://semantic-domain.blogspot.com/2023/07/linear-time-parser-combinators.html}
      Linear-time parser combinators} by Neel Krishnaswami
      ({{:https://gist.github.com/neel-krishnaswami/b1594c57433b7df2a143634a2fff3544}
      Source code})
    - {{:https://www.cl.cam.ac.uk/~nk480/parsing.pdf} A Typed, Algebraic
      Approach to Parsing} by Neel Krishnaswami and Jeremy Yallop
*)

module Tp : sig
  (** The semantic types of parsers *)

  type t = {
    null : bool;
    (** The {i nullability predicate}, i.e. whether the parser might succeed
        while consuming no input. *)

    first : Byte_set.t;
    (** The {i first set}, i.e. the set of bytes that can appear as the first
        byte of this parser. *)

    follow : Byte_set.t;
    (** The {i follow set}, i.e. the set of bytes that can appear at the first
        byte of each suffix. *)
  }

  exception Type_error of string


  val empty : t
  (** [empty] is the type of the language containing only the empty string. *)

  val byte : char -> t
  (** [byte c] is the type of the singleton language containing the byte [b]. *)

  val byte_set : Byte_set.t -> t
  (** [byte_set s] is the type of the language containg single byte strings
      corresponding to each element of [s]. *)

  val bytes : bytes -> t
  (** [bytes s] is the type of the language containing the byte string [s]. *)

  val fail : t
  (** [fail] is the type of the language with no strings. *)

  val seq : t -> t -> t
  (** [seq t1 t2] is the type of the language produced by sequencing a language
      of type [t1] before a language of type [t2]. *)

  val alt : t -> t -> t
  (** [alt t1 t2] is the type of the language produced from the union of a
      language of type [t1] with a language of type [t2]. *)

  val fix : (t -> t) -> t
  (** [fix f] returns the smallest type [t] such that [f t = t]. *)

  val equal : t -> t -> bool
  (** [equal t1 t2] checks to see if [t1] is equal to [t2] *)

  val pp_print : Format.formatter -> t -> unit
  (** [pp_print ppf t] pretty prints [t] using the formatter [ppf] *)

end = struct

  type t = {
    null : bool;
    first : Byte_set.t;
    follow : Byte_set.t;
  }

  let pp_print ppf t =
    Format.fprintf ppf "{\n";
    Format.fprintf ppf "   null = %a;\n" Format.pp_print_bool t.null;
    Format.fprintf ppf "   first = Byte_set.of_string \"%a\";\n" Byte_set.pp_print t.first;
    Format.fprintf ppf "   follow = Byte_set.of_string \"%a\";\n" Byte_set.pp_print t.follow;
    Format.fprintf ppf "}\n"

  exception Type_error of string

  let empty = {
    null = true; (* Never consumes any input *)
    first = Byte_set.empty;
    follow = Byte_set.empty;
  }

  let byte_set s = {
    null = false; (* Always consumes exactly one byte from the input *)
    first = s;
    follow = Byte_set.empty;
  }

  let byte b = byte_set (Byte_set.singleton b)
  let fail = byte_set Byte_set.empty

  let bytes s =
    if Bytes.length s = 0 then
      empty
    else
      byte (Bytes.get s 0)

  (** [separate t1 t2] checks that the follow set of [t1] type does not
      overlap with the first set of [t1]. This is important to ensure that we
      know for certain when to stop parsing a parser with type [t1], and to
      start parsing a parser of type [t2] without needing to backtrack. *)
  let separate t1 t2 =
    (* TODO: Could it be ok for either [t1] or [t2] to be nullable? *)
    not t1.null && Byte_set.disjoint t1.follow t2.first

  (** [non_overlapping t1 t2] checks if the two types can be uniquely
      distinguished based on their first sets. This is important to avoid
      ambiguities in alternation and hence avoid backtracking. *)
  let non_overlapping t1 t2 =
    not (t1.null && t2.null) && Byte_set.disjoint t1.first t2.first

  let seq t1 t2 =
    if separate t1 t2 then
      {
        null = false;
        first = t1.first;
        follow =
          Byte_set.union
            t2.follow
            (if t2.null
              then Byte_set.union t2.first t1.follow
              else Byte_set.empty);
      }
    else begin
      raise (Type_error "ambiguous sequencing")
    end

  let alt t1 t2 =
    if non_overlapping t1 t2 then
      {
        null = t1.null || t2.null;
        first = Byte_set.union t1.first t2.first;
        follow = Byte_set.union t1.follow t2.follow;
      }
    else
      raise (Type_error "ambiguous alternation")

  let equal t1 t2 =
    t1.null = t2.null
      && Byte_set.equal t1.first t2.first
      && Byte_set.equal t1.follow t2.follow

  let fix f =
    let rec loop t =
      let t' = f t in
      if equal t t' then
        t'
      else
        (loop [@tailcall]) t'
    in
    loop fail

end

module Parser: sig

  type 'a t

  exception Parse_failure of int

  val map : ('a -> 'b) -> 'a t -> 'b t

  val unit : unit t
  val return : 'a -> 'a t

  val seq : 'a t -> 'b t -> ('a * 'b) t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  val byte : char -> unit t
  val byte_set : Byte_set.t -> char t
  val bytes : bytes -> unit t

  val fail : 'a t
  val alt : 'a t -> 'a t -> 'a t
  val one_of : 'a t list -> 'a t
  val fix : ('a t -> 'a t) -> 'a t

  val parse : 'a t -> bytes -> int -> int * 'a

end = struct

  type 'a t = {
    tp : Tp.t;
    parse : bytes -> int -> int * 'a;
  }

  exception Parse_failure of int

  let map f p = {
    tp = p.tp;
    parse = fun input pos ->
      let pos, v = p.parse input pos in
      pos, f v;
  }

  let unit = {
    tp = Tp.empty;
    parse = fun _ pos -> pos, ();
  }

  let return x = {
    tp = Tp.empty;
    parse = fun _ pos -> pos, x;
  }

  let seq p1 p2 = {
    tp = Tp.seq p1.tp p2.tp;
    parse = fun input pos ->
      let pos, a = p1.parse input pos in
      let pos, b = p2.parse input pos in
      pos, (a, b);
  }

  let ( let+ ) p f = map f p
  let ( and+ ) = seq

  let fail = {
    tp = Tp.fail;
    parse = fun _ pos -> raise (Parse_failure pos);
  }

  let get_byte input pos =
    if pos < Bytes.length input then
      Some (Bytes.unsafe_get input pos)
    else
      None

  let byte b = {
    tp = Tp.byte b;
    parse = fun input pos ->
      match get_byte input pos with
      | Some b' when b' = b -> pos + 1, ()
      | _ -> raise (Parse_failure pos);
  }

  let byte_set s = {
    tp = Tp.byte_set s;
    parse = fun input pos ->
      match get_byte input pos with
      | Some b when Byte_set.mem b s -> pos + 1, b
      | _ -> raise (Parse_failure pos);
  }

  let bytes s = {
    tp = Tp.bytes s;
    parse = fun input pos ->
      if pos + Bytes.length s < Bytes.length input then
        let rec go off =
          match get_byte s off with
          | Some b when b = Bytes.get input (pos + off) -> (go [@tailcall]) (off + 1)
          | Some _ -> raise (Parse_failure (pos + off))
          | None -> pos + off, ()
        in
        go 0
      else
        raise (Parse_failure pos);
  }

  let alt p1 p2 = {
    tp = Tp.alt p1.tp p2.tp;
    parse = fun input pos ->
      match get_byte input pos with
      | Some b when Byte_set.mem b p1.tp.Tp.first -> p1.parse input pos
      | Some b when Byte_set.mem b p2.tp.Tp.first -> p2.parse input pos
      | _ when p1.tp.Tp.null -> p1.parse input pos
      | _ when p2.tp.Tp.null -> p2.parse input pos
      | _ -> raise (Parse_failure pos);
  }

  let one_of ps =
    (* TODO: Compile to lookup map *)
    List.fold_left alt fail ps

  let fix f =
    let g tp = (f { fail with tp }).tp in
    let r = ref fail.parse in
    let p = f {
      tp = Tp.fix g;
      parse = fun input pos -> !r input pos;
    } in
    r := p.parse;
    (* Format.printf "%a" Tp.pp_print p.tp; *)
    p

  let parse p =
    p.parse

end

module Parser_util : sig
  (** Derived parsers *)

  open Parser

  val ( ==> ) : 'a t -> ('a -> 'b) -> 'b t
  val ( ++ ) :  'a t -> 'b t -> ('a * 'b) t

  val ignore_left : 'a t -> 'b t -> 'b t
  val ignore_right : 'a t -> 'b t -> 'a t

  val ( <* ) : 'a t -> 'b t -> 'a t
  val ( *> ) : 'a t -> 'b t -> 'b t

  val many0 : 'a t -> 'a list t
  val many1 : 'a t -> 'a list t

  val skip_many0 : 'a t -> unit t
  val skip_many1 : 'a t -> unit t

  val sep0 : 'a t -> 'b t -> 'a list t
  val sep1 : 'a t -> 'b t -> 'a list t

end = struct

  open Parser

  let ( ==> ) p f = map f p
  let ( ++ ) = seq

  let ignore_left p1 p2 =
    let+ _ = p1
    and+ x = p2 in
    x

  let ignore_right p1 p2 =
    let+ x = p1
    and+ _ = p2 in
    x

  let ( <* ) = ignore_right
  let ( *> ) = ignore_left

  let many0 (p : 'a t) : 'a list t =
    fix (fun r -> one_of [
      unit ==> (fun () -> []);
      seq p r ==> (fun (x, xs) -> x :: xs);
    ])

  let skip_many0 (p : 'a t) : unit t =
    fix (fun r -> one_of [
      unit ==> (fun () -> ());
      seq p r ==> (fun (_, _) -> ());
    ])

  let many1 (p : 'a t) : 'a list t =
    let+ x = p
    and+ xs = fix (fun r -> one_of [
      unit ==> (fun () -> []);
      seq p r ==> (fun (x, xs) -> x :: xs);
    ]) in
    x :: xs

  let skip_many1 (p : 'a t) : unit t =
    let+ _ = p
    and+ () = fix (fun r -> one_of [
      unit ==> (fun () -> ());
      seq p r ==> (fun (_, _) -> ());
    ]) in
    ()

  let sep1 p s =
    let+ x = p
    and+ xs = many0 (s *> p) in
    x :: xs

  let sep0 p s = one_of [
    unit ==> (fun () -> []);
    sep1 p s;
  ]

end
