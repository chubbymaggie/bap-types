open Core_kernel.Std

type t = Arbitrary of Z.t * int
type bv = t

let normalize = function
  | Arbitrary (z, t) -> Arbitrary (Z.logand z Z.((shift_left one t) - one), t)
let lit n t   = normalize (Arbitrary (Z.of_int n, t))
let lit64 n t = normalize (Arbitrary (Z.of_int64 n, t))
let litz n t  = normalize (Arbitrary (n, t))

let to_string = function
  | Arbitrary (z, 1) ->
    if Z.equal z Z.zero then "false" else "true"
  | Arbitrary (z, n) ->
    Z.to_string z ^ ":" ^ string_of_int n

let to_hex (Arbitrary (z, _)) =
  let hex_string = Z.format "%x" z in
  "0x" ^ hex_string
(* this function probably should not exist if and when all occurrences of
 *  * Z.t are replaced with Bitvector.t *)
let hex_of_z z = "0x" ^ (Z.format "%x" z)

let bv_true = lit 1 1
let bv_false = lit 0 1

let bool_of = function
  | Arbitrary (z, _) ->
    not (Z.equal Z.zero z)

let rec normalize_bytes n = function
  | [] -> if n > 0 then '\000' :: (normalize_bytes (n-1) []) else []
  | (hd::tl) ->
    if n > 0 then hd :: (normalize_bytes (n-1) tl)
    else []

let bytes_of (Arbitrary (z, t)) =
  normalize_bytes ((t + 7) / 8) (String.to_list (Z.to_bits z))

let of_bytes bs =
  litz (Z.of_bits (String.of_char_list bs)) (8 * List.length bs)

let concat (Arbitrary (z1, t1)) (Arbitrary (z2, t2)) =
  let lhs = Z.shift_left z1 t2 in
  litz (Z.add lhs z2) (t1 + t2)

let neg (Arbitrary (z, t)) =
  litz (Z.neg z) t

let lognot (Arbitrary (z, t)) =
  litz (Z.lognot z) t

let compare l r =
  let Arbitrary (z1, _) = normalize l in
  let Arbitrary (z2, _) = normalize r in
  Z.compare z1 z2

let width_of (Arbitrary (_, t)) = t

exception Width

(** Apply a binary operation, first asserting
  * that the inputs have the same length. *)
let checked_binop op (Arbitrary (z1, t1)) (Arbitrary (z2, t2)) =
  if t1 <> t2 then raise Width
  else litz (op z1 z2) t1

let logand  = checked_binop Z.logand
let logor   = checked_binop Z.logor
let logxor  = checked_binop Z.logxor
let plus    = checked_binop Z.add
let minus   = checked_binop Z.sub
let times   = checked_binop Z.mul
let divide  = checked_binop Z.ediv
let sdivide = checked_binop Z.div

(** Apply a test, but yield a width-1 bitvector, not a boolean.
  * Also checks that inputs have the same length. *)
let checked_test op (Arbitrary (z1, t1)) (Arbitrary (z2, t2)) =
  if t1 <> t2 then raise Width
  else if op z1 z2 then bv_true
  else bv_false

let eq = checked_test Z.equal
let neq = checked_test (fun a b -> not (Z.equal a b))
let lt = checked_test Z.lt
let le = checked_test Z.leq

(** Extend the sign bit.
  * This makes Zarith treat the normalized input as if it were signed. *)
let extend_sign (Arbitrary (z, t)) =
  Z.signed_extract z 0 t

let slt l r =
  let (Arbitrary (_, t1)), (Arbitrary (_, t2)) = l, r in
  if t1 <> t2 then raise Width
  else if Z.lt (extend_sign l) (extend_sign r) then bv_true
  else bv_false

let sle l r =
  let (Arbitrary (_, t1)), (Arbitrary (_, t2)) = l, r in
  if t1 <> t2 then raise Width
  else if Z.leq (extend_sign l) (extend_sign r) then bv_true
  else bv_false

(* FIXME These three fns should probably check for overflow,
 * even if z2 should always be small. *)
let lshift (Arbitrary (z1, t1)) (Arbitrary (z2, _)) =
  litz (Z.shift_left z1 (Z.to_int z2)) t1

let rshift (Arbitrary (z1, t1)) (Arbitrary (z2, _)) =
  litz (Z.shift_right z1 (Z.to_int z2)) t1

let arshift ((Arbitrary (z1, t1)) as v) (Arbitrary (z2, _)) =
  litz (Z.shift_right (extend_sign v) (Z.to_int z2)) t1

let cast_unsigned (Arbitrary (z1, t1)) t2 =
  litz z1 t2

let cast_signed v t =
  litz (extend_sign v) t

let cast_high (Arbitrary (z1, t1)) t2 =
  litz (Z.shift_right z1 (t1 - t2)) t2

let cast_low = cast_unsigned

let unsigned_mod = checked_binop Z.erem

let signed_mod (Arbitrary (z1, t1) as l) (Arbitrary (z2, t2) as r) =
  if t1 <> t2 then raise Width
  else litz (Z.rem (extend_sign l) (extend_sign r)) t1

let extract h l (Arbitrary (z, _)) =
  let z = (Z.shift_right z l) in
  litz z (h - l + 1)

let decr (Arbitrary (z, t)) =
  litz Z.(z - one) t

let incr (Arbitrary (z, t)) =
  litz Z.(z + one) t

let to_zarith (Arbitrary (z, t)) = z
