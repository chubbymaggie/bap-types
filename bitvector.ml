open Z
type t = Arbitrary of Z.t * int
type bv = t
let normalize i =
  match i with
  | Arbitrary (z, t) -> Arbitrary (z mod ~$t, t)
let lit n t = normalize (Arbitrary (~$n, t))
