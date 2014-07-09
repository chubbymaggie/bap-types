(* runtime support for OCaml custom types used in BAP *)
open Core_kernel.Std

type char = Char.t
let char_of_int = Char.of_int
let char_to_int = Char.to_int

type bitvector = Bitvector.t
let bitvector_of_char_list = Bitvector.of_bytes
let bitvector_to_char_list = Bitvector.to_bytes
