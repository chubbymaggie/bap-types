open Type
open Var
open Bil

type arch =
  | X86_32
  | X86_64
  | ARM

let to_string = function
  | X86_32 -> "X86_32"
  | X86_64 -> "X86_64"
  | ARM    -> "ARM"

let of_string = function
  | "x86" | "x86_32" | "X86_32" -> X86_32
  | "x86-64" | "X86-64" -> X86_64
  | "arm" | "ARM" -> ARM
  | _ -> failwith "Arch.of_string: Unknown arch"

exception Arch_exception of arch * string

module type ARCH =
sig
  type cpustate
  val arch : arch
  val mem_index_type : typ
  val mem : var
  val sp  : var
  val ip  : var
  val init_state : cpustate
  val state_with_addr : cpustate
    -> addr
    -> cpustate
  val regs : var list
  val disasm : cpustate
    -> (addr -> char)
    -> addr
    -> cpustate * stmt list * addr
end
