open Z
type t = Arbitrary of Z.t * int
type bv = t
let normalize = function
  | Arbitrary (z, t) -> Arbitrary (z mod ~$t, t)
let lit n t = normalize (Arbitrary (~$n, t))
let to_string = function
  | Arbitrary (z, 1) ->
    if Z.equal z ~$0
    then "false"
    else "true"
  | Arbitrary (z, n) ->
    Z.to_string z ^ ":" ^ string_of_int n
