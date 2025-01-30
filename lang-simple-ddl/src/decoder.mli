type 'a t

val run : 'a. 'a t -> input:bytes -> pos:int -> (int * 'a, int) result

val pure : 'a. 'a -> 'a t
val map : 'a 'b. ('a -> 'b) -> 'a t -> 'b t
val both : 'a 'b. 'a t -> 'b t -> ('a * 'b) t
val branch : 'a1 'a2 'b. ('a1, 'a2) Either.t t -> ('a1 -> 'b) t -> ('a2 -> 'b) t -> 'b t
val bind : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t
val fail : 'a. 'a t

val byte : char t
val repeat_len : 'a. int64 -> 'a t -> 'a list t
