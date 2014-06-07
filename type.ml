(** The IR type of a BIL expression *)
type typ =
  | Reg of int (** an N-bit bitvector (use 1 for booleans). *)
  | TMem of typ * typ (** Memory of given index type, element type. *)

(** Different forms of casting *)
type cast_type =
  | CAST_UNSIGNED (** 0-padding widening cast. *)
  | CAST_SIGNED (** Sign-extending widening cast. *)
  | CAST_HIGH (** Narrowning cast. Keeps the high bits. *)
  | CAST_LOW (** Narrowing cast. Keeps the low bits. *)

(** Binary operations implemented in the IR *)
type binop_type =
  | PLUS (** Integer addition. (commutative, associative) *)
  | MINUS (** Subtract second integer from first. *)
  | TIMES (** Integer multiplication. (commutative, associative)*)
  | DIVIDE (** Unsigned integer division. *)
  | SDIVIDE (** Signed integer division. *)
  | MOD (** Unsigned modulus. *)
  | SMOD (** Signed modulus. *)
  | LSHIFT (** Left shift. *)
  | RSHIFT (** Right shift, fill with 0. *)
  | ARSHIFT (** Right shift, sign extend. *)
  | AND (** Bitwise and. (commutative, associative) *)
  | OR (** Bitwise or. (commutative, associative) *)
  | XOR (** Bitwise xor. (commutative, associative) *)
  | EQ (** Equals (commutative) (associative on booleans) *)
  | NEQ (** Not equals (commutative) (associative on booleans) *)
  | LT (** Unsigned less than *)
  | LE (** Unsigned less than or equal to *)
  | SLT (** Signed less than *)
  | SLE (** Signed less than or equal to *)

(** Unary operations implemented in the IR *)
type unop_type =
  | NEG (** Negate (2's complement) *)
  | NOT (** Bitwise not *)

type addr = Bitvector.t
