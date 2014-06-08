open Type

module BV = Bitvector
module F  = Format

let rec string_of_typ = function
  | Reg 1 -> "bool"
  | Reg n -> Printf.sprintf "u%u" n
  | TMem (idx,e) -> string_of_typ idx ^ "?" ^ string_of_typ e

let string_of_ct = function
  | CAST_UNSIGNED  -> "pad"
  | CAST_SIGNED -> "extend"
  | CAST_HIGH -> "high"
  | CAST_LOW -> "low"

let string_of_binop = function
  | PLUS -> "+"
  | MINUS -> "-"
  | TIMES -> "*"
  | DIVIDE -> "/"
  | SDIVIDE -> "$/"
  | MOD -> "%"
  | SMOD -> "$%"
  | LSHIFT -> "<<"
  | RSHIFT -> ">>"
  | ARSHIFT -> "$>>"
  | AND -> "&"
  | OR -> "|"
  | XOR -> "^"
  | EQ -> "=="
  | NEQ -> "<>"
  | LT -> "<"
  | LE -> "<="
  | SLT -> "$<"
  | SLE -> "$<="

let string_of_unop = function
  | NEG -> "-"
  | NOT -> "~"

let string_of_endian = function
  | Bil.LittleEndian -> "el"
  | Bil.BigEndian    -> "be"

let string_of_var (Var.V(id,name,t)) =
  name ^ "_" ^ string_of_int id ^ ":" ^ string_of_typ t

let rec string_of_exp = function
  | Bil.Load   (mem, idx, edn, t) ->
    string_of_exp mem ^ "[" ^ string_of_exp idx ^ ", " ^
    string_of_endian edn ^ "]:" ^ string_of_typ t
  | Bil.Store  (mem, idx, v, edn, t) ->
    string_of_exp mem ^ " with [" ^ string_of_exp idx ^ ", " ^
    string_of_endian edn ^ "]:" ^ string_of_typ t ^ " = " ^ string_of_exp v
  | Bil.Ite    (c, t, f) ->
    "if " ^ string_of_exp c ^ " then " ^ string_of_exp t ^ " else " ^
    string_of_exp f
  | Bil.Extract(h, l, e) ->
    "extract:" ^ string_of_int h ^ ":" ^ string_of_int l ^ "[" ^ string_of_exp e ^ "]"
  | Bil.Concat (le, re) ->
    "concat[" ^ string_of_exp le ^ ":" ^ string_of_exp re ^ "]"
  | Bil.BinOp  (op, le, re) ->
    "(" ^ string_of_exp le ^ ") " ^ string_of_binop op ^ " (" ^ string_of_exp re ^ ")"
  | Bil.UnOp   (op, e) ->
    string_of_unop op ^ "(" ^ string_of_exp e ^ ")"
  | Bil.Var    v -> string_of_var v
  | Bil.Int    i -> BV.to_string i
  | Bil.Cast   (ct, t, e) ->
    string_of_ct ct ^ ":" ^ string_of_typ t ^ "[" ^ string_of_exp e ^ "]"
  | Bil.Let    (v, def, body) ->
    "let " ^ string_of_var v ^ " = " ^ string_of_exp def ^ " in " ^ string_of_exp body
  | Bil.Unknown(s, t) ->
    "unknown{" ^ s ^ "}:" ^ string_of_typ t

let string_of_bil = function
  | Bil.Move(v, e) ->
    string_of_var v ^ " = " ^ string_of_exp e
  | Bil.Jmp e ->
    "jmp " ^ string_of_exp e
  | Bil.CJmp(c, t, f) ->
    "cjmp {cond=" ^ string_of_exp c ^ ", true=" ^ string_of_exp t ^ ", false="
    ^ string_of_exp f ^ "}"
  | Bil.Special s ->
    "special: " ^ s
