type t
exception Width

val lit : int -> int -> t
val lit64 : int64 -> int -> t
val litz : Z.t -> int -> t
val to_string : t -> string
val to_hex : t -> string
val hex_of_z : Z.t -> string
val bv_true : t
val bv_false : t
val bool_of : t -> bool
val bytes_of : t -> Core_kernel.Std.String.elt list
val of_bytes : char Core_kernel.Std.List.t -> t
val concat : t -> t -> t
val neg : t -> t
val lognot : t -> t
val compare : t -> t -> int
val width_of : t -> int
val logand : t -> t -> t
val logor : t -> t -> t
val logxor : t -> t -> t
val plus : t -> t -> t
val minus : t -> t -> t
val times : t -> t -> t
val divide : t -> t -> t
val sdivide : t -> t -> t
val eq : t -> t -> t
val neq : t -> t -> t
val lt : t -> t -> t
val le : t -> t -> t
val slt : t -> t -> t
val sle : t -> t -> t
val lshift : t -> t -> t
val rshift : t -> t -> t
val arshift : t -> t -> t
val cast_unsigned : t -> int -> t
val cast_signed : t -> int -> t
val cast_high : t -> int -> t
val cast_low : t -> int -> t
val unsigned_mod : t -> t -> t
val signed_mod : t -> t -> t
val extract : int -> int -> t -> t
val decr : t -> t
val incr : t -> t
val to_zarith : t -> Z.t
