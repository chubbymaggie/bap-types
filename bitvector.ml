open Z
type t = Arbitrary of Z.t * int
type bv = t
let normalize = function
  | Arbitrary (z, t) -> Arbitrary (z mod ~$t, t)
let lit n t   = normalize (Arbitrary (~$n, t))
let lit64 n t = normalize (Arbitrary (Z.of_int64 n, t))
let litz n t  = normalize (Arbitrary (n, t))
let to_string = function
  | Arbitrary (z, 1) ->
    if Z.equal z ~$0 then "false" else "true"
  | Arbitrary (z, n) ->
    Z.to_string z ^ ":" ^ string_of_int n

let to_hex (Arbitrary (z, _)) =
  let hex_string = Z.format "%x" z in
  "0x" ^ hex_string
(* this function probably should not exist if and when all occurrences of
 *  * Z.t are replaced with Bitvector.t *)
let hex_of_z z = "0x" ^ (Z.format "%x" z)
