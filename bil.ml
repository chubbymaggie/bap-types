open Type

type endian = LittleEndian | BigEndian

type exp =
  | Load    of exp * exp * endian * typ
  (** Load    (arr,  idx,  endian,  t) *)
  | Store   of exp * exp * exp * endian * typ
  (** Store   (arr,  idx,  val,  endian,  t) *)
  | BinOp   of binop_type * exp * exp
  | UnOp    of unop_type * exp
  | Var     of Var.t
  | Int     of Bitvector.t
  | Cast    of cast_type * typ * exp
  (** Cast to a new type *)
  | Let     of Var.t * exp * exp
  | Unknown of string * typ
  (* Expression types below here are just syntactic sugar for the above *)
  | Ite     of exp * exp * exp
  | Extract of int * int * exp
  (** Extract hbits to lbits of e (Reg type) *)
  | Concat  of exp * exp
  (** Concat two reg expressions together *)

type stmt =
  | Move    of Var.t * exp
  (** Assign the value on the right to the var on the left *)
  | Jmp     of exp
  (** Jump to a address *)
  | Special of string
  (** Statement with semantics not expressible in BIL *)
  | While   of exp * stmt list
  | If      of exp * stmt list * stmt list
  | CpuExn  of int

module Bop = struct
  (** Arithmetic operations *)
  let ( + )    a b   = BinOp (PLUS,    a, b)
  let ( - )    a b   = BinOp (MINUS,   a, b)
  let ( * )    a b   = BinOp (TIMES,   a, b)
  let ( / )    a b   = BinOp (DIVIDE,  a, b)
  let ( /$ )   a b   = BinOp (SDIVIDE, a, b)
  let ( mod )  a b   = BinOp (MOD,     a, b)
  let ( %$ )   a b   = BinOp (SMOD,    a, b)

  (** Bit operations *)
  let ( lsl )  a b   = BinOp (LSHIFT,  a, b)
  let ( lsr )  a b   = BinOp (RSHIFT,  a, b)
  let ( asr )  a b   = BinOp (ARSHIFT, a, b)
  let ( land ) a b   = BinOp (AND,     a, b)
  let ( lor )  a b   = BinOp (OR,      a, b)
  let ( lxor ) a b   = BinOp (XOR,     a, b)
  let lnot     a     = UnOp  (NOT,     a)

  (** Equality tests *)
  let ( = )    a b   = BinOp (EQ,      a, b)
  let ( <> )   a b   = BinOp (NEQ,     a, b)
  let ( < )    a b   = BinOp (LT,      a, b)
  let ( > )    a b   = BinOp (LT,      b, a)
  let ( <= )   a b   = BinOp (LE,      a, b)
  let ( >= )   a b   = BinOp (LE,      b, a)
  let ( <$ )   a b   = BinOp (SLT,     a, b)
  let ( >$ )   a b   = BinOp (SLT,     b, a)
  let ( <=$ )  a b   = BinOp (SLE,     a, b)
  let ( >=$ )  a b   = BinOp (SLE,     b, a)

  (** Misc operations *)
  let ( ^ )    a b   = Concat (a, b)
end
