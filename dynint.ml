open Z
type dynint = Arbitrary of (t * int)
let normalize i =
  match i with
  | Arbitrary (z, t) -> Arbitrary (z mod ~$t, t)
let lit n t = normalize (Arbitrary (~$n, t))
