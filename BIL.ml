open Var
open Type
open Dynint

type exp =
  | Load    of exp * exp * exp * typ       (** Load(arr,idx,endian,t) *)
  | Store   of exp * exp * exp * exp * typ (** Store(arr,idx,val,endian,t) *)
  | BinOp   of binop_type * exp * exp
  | UnOp    of unop_type * exp
  | Var     of var
  | Int     of dynint
  | Cast    of cast_type * typ * exp       (** Cast to a new type. *)
  | Let     of var * exp * exp
  | Unknown of string * typ
  (* Expression types below here are just syntactic sugar for the above *)
  | Ite     of exp * exp * exp
  | Extract of int * int * exp             (** Extract hbits to lbits of e (Reg type) *)
  | Concat  of exp * exp                   (** Concat two reg expressions together *)

type stmt =
  | Move    of var * exp (** Assign the value on the right to the
                                     var on the left *)
  | Jmp     of exp       (** Jump to a label/address *)
  | CJmp    of exp * exp * exp
  (** Conditional jump. If e1 is true, jumps to e2, otherwise jumps to e3 *)
  | Special of string (** Statement with semantics not expressible in BIL *)

(** False constant. *)
let exp_false = Int(lit 0 1)
(** True constant. *)
let exp_true = Int(lit 1 1)

let little_endian = exp_false
let big_endian = exp_true