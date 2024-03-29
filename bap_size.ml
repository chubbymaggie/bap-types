open Core_kernel.Std
open Or_error

open Bap_common
open Reg

let to_bits : 'a -> int = function
  | `r8   -> 8
  | `r16  -> 16
  | `r32  -> 32
  | `r64  -> 64
  | `r128 -> 128
  | `r256 -> 256

let to_bytes x = to_bits x / 8

let of_int : int -> size Or_error.t = function
  | 8   -> Ok `r8
  | 16  -> Ok `r16
  | 32  -> Ok `r32
  | 64  -> Ok `r64
  | 128 -> Ok `r128
  | 256 -> Ok `r256
  | n   -> errorf "unsupported word size: %d" n

let of_int_exn n = ok_exn (of_int n)

let of_int_opt n = Result.ok (of_int n)

let addr_of_word_size = function
  | Word_size.W32 -> `r32
  | Word_size.W64 -> `r64

let of_addr_size (x : addr_size) : size = (x :> size)

let to_addr_size (x : size) : addr_size Or_error.t =
  match x with
  | `r32 | `r64 as x -> Ok x
  | n -> errorf "unsupported address size: %d" (to_bits n)

let addr_of_int : int -> addr_size Or_error.t = function
  | 32 -> Ok `r32
  | 64 -> Ok `r64
  | n  -> errorf "unsupported address size: %d" n


let addr_of_int_exn n = ok_exn (addr_of_int n)

let addr_of_int_opt n = Result.ok (addr_of_int n)


module T = struct
  open Format

  type t = size with bin_io, compare, sexp
  let module_name = "Bap_types.Bap_size"

  let pp fmt n =
    fprintf fmt "u%u" (to_bits n)

  let hash = to_bytes
end

include Regular.Make(T)
