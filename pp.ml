open Type

module BV = Bitvector
module F  = Format

let rec string_of_typ = function
  | Reg 1 -> "bool"
  | Reg n -> Printf.sprintf "u%u" n
  | TMem (idx, elm) -> string_of_typ idx ^ "?" ^ string_of_typ elm

let string_of_ct = function
  | CAST_UNSIGNED -> "pad"
  | CAST_SIGNED   -> "extend"
  | CAST_HIGH     -> "high"
  | CAST_LOW      -> "low"

let string_of_binop = function
  | PLUS    -> "+"
  | MINUS   -> "-"
  | TIMES   -> "*"
  | DIVIDE  -> "/"
  | SDIVIDE -> "$/"
  | MOD     -> "%"
  | SMOD    -> "$%"
  | LSHIFT  -> "<<"
  | RSHIFT  -> ">>"
  | ARSHIFT -> "$>>"
  | AND     -> "&"
  | OR      -> "|"
  | XOR     -> "^"
  | EQ      -> "=="
  | NEQ     -> "<>"
  | LT      -> "<"
  | LE      -> "<="
  | SLT     -> "$<"
  | SLE     -> "$<="

let string_of_unop = function
  | NEG -> "-"
  | NOT -> "~"

let string_of_endian = function
  | Bil.LittleEndian -> "el"
  | Bil.BigEndian    -> "be"

let string_of_var (Var.V (id, name, typ)) =
  name ^ "_" ^ string_of_int id ^ ":" ^ string_of_typ typ

let rec string_of_exp = function

  | Bil.Load    (mem, idx, edn, typ) ->
    string_of_exp mem ^ "[" ^ string_of_exp idx ^ ", " ^
    string_of_endian edn ^ "]:" ^ string_of_typ typ

  | Bil.Store   (mem, idx, exp, edn, typ) ->
    string_of_exp mem ^ " with [" ^ string_of_exp idx ^ ", " ^
    string_of_endian edn ^ "]:" ^ string_of_typ typ ^ " = " ^ string_of_exp exp

  | Bil.Ite     (ce, te, fe) ->
    "if " ^ string_of_exp ce ^
    " then " ^ string_of_exp te ^
    " else " ^ string_of_exp fe

  | Bil.Extract (hi, lo, exp) ->
    "extract:" ^ string_of_int hi ^ ":" ^ string_of_int lo ^
    "[" ^ string_of_exp exp ^ "]"

  | Bil.Concat  (le, re) ->
    "concat[" ^ string_of_exp le ^ ":" ^ string_of_exp re ^ "]"

  | Bil.BinOp   (op, le, re) ->
    "(" ^ string_of_exp le ^ ") " ^
    string_of_binop op ^
    " (" ^ string_of_exp re ^ ")"

  | Bil.UnOp    (op, exp) ->
    string_of_unop op ^ "(" ^ string_of_exp exp ^ ")"

  | Bil.Var     var -> string_of_var var

  | Bil.Int     bv -> BV.to_string bv

  | Bil.Cast    (ct, typ, exp) ->
    string_of_ct ct ^ ":" ^ string_of_typ typ ^ "[" ^ string_of_exp exp ^ "]"

  | Bil.Let     (var, def, body) ->
    "let " ^ string_of_var var ^ " = " ^ string_of_exp def ^ " in " ^
    string_of_exp body

  | Bil.Unknown (s, typ) ->
    "unknown{" ^ s ^ "}:" ^ string_of_typ typ

let string_of_bil = function

  | Bil.Move    (var, exp) ->
    string_of_var var ^ " = " ^ string_of_exp exp

  | Bil.Jmp     exp ->
    "jmp " ^ string_of_exp exp

  | Bil.CJmp    (ce, te, fe) ->
    "cjmp {cond=" ^ string_of_exp ce ^
    ", true=" ^ string_of_exp te ^
    ", false=" ^ string_of_exp fe ^ "}"

  | Bil.Special s ->
    "special: " ^ s
